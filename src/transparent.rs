use crate::to_wstring;
use emacs::{defun, Result};
use std::sync::atomic::AtomicU16;
use std::sync::Arc;
use std::sync::Mutex;
use winapi::shared::ntdef::NULL;
use winapi::shared::winerror::ERROR_SUCCESS;
use winapi::um::errhandlingapi::GetLastError;
use winapi::um::errhandlingapi::SetLastError;
use winapi::um::libloaderapi::*;
use winapi::um::wingdi::*;
extern crate native_windows_gui as nwg;
use std::collections::HashMap;
use std::sync::atomic::Ordering;
use winapi::shared::basetsd::LONG_PTR;
use winapi::shared::minwindef::*;
use winapi::shared::windef::*;
use winapi::um::wingdi::RGB;
use winapi::um::winuser::*;

// 查看过release，确实把format!给优化了，没有内存分配
macro_rules! debug_output {
    ($str:expr) => {
        #[cfg(debug_assertions)]
        #[allow(unused_unsafe)]
        unsafe {
            use winapi::um::debugapi::*;
            use winapi::um::processthreadsapi::*;
            $str.insert_str(
                0,
                &format!(
                    "pid:{}, tid:{} ",
                    GetCurrentProcessId(),
                    GetCurrentThreadId()
                ),
            );
            let s = to_wstring(&$str);
            OutputDebugStringW(s.as_ptr() as *const _);
        }
    };
}

// https://github.com/duilib/duilib/blob/bbc817e0a134cda1dc5be6a38864257649273095/DuiLib/Utils/WndShadow.cpp#L197-L307
// msdn: https://docs.microsoft.com/en-us/windows/win32/winmsg/window-features
pub fn transparent_init() {
    unsafe {
        let cls_name = to_wstring("EmacsShadowWindow");
        let wc = WNDCLASSEXW {
            cbSize: std::mem::size_of::<WNDCLASSEXW>() as UINT,
            style: CS_HREDRAW | CS_VREDRAW,
            lpfnWndProc: Some(cwndshadow_window_proc),
            cbClsExtra: 0,
            cbWndExtra: 0,
            hInstance: GetModuleHandleW(std::ptr::null_mut()) as HINSTANCE,
            hIcon: std::ptr::null_mut(),
            hCursor: LoadCursorW(std::ptr::null_mut(), IDC_ARROW),
            hbrBackground: GetStockObject(WHITE_BRUSH as i32) as HBRUSH,
            lpszMenuName: std::ptr::null_mut(),
            lpszClassName: cls_name.as_ptr(),
            hIconSm: std::ptr::null_mut(),
        };
        if RegisterClassExW(&wc) == 0 {
            #[cfg(debug_assertions)]
            {
                //OutputDebugStringA(b"RegisterClassEx failed\0".as_ptr() as *const _);
            }
        }
    }
}

struct EnumData {
    pid: DWORD,
    ret: Vec<HWND>,
}
fn enum_windows_item(data: &mut EnumData, hwnd: HWND) -> BOOL {
    let mut p: DWORD = 0;
    if unsafe { GetWindowThreadProcessId(hwnd, &mut p as *mut _) != 0 } && p == data.pid {
        unsafe {
            let mut buf: [u8; MAX_PATH] = std::mem::zeroed();
            let len = GetClassNameA(hwnd, &mut buf as *mut _ as *mut _, MAX_PATH as i32);
            if len == 5 && &buf[0..5] == b"Emacs" {
                data.ret.push(hwnd);
            }
        }
    }
    1
}
unsafe extern "system" fn enum_callback(win_hwnd: HWND, arg: LONG_PTR) -> BOOL {
    let pthis = arg as *mut EnumData;
    enum_windows_item(&mut *pthis, win_hwnd)
}

struct ShadowData {
    hwnd: usize,
    width: AtomicU16, // WORD,
    height: AtomicU16,
    xpos: AtomicU16,
    ypos: AtomicU16,
    org_winproc: usize, //WNDPROC,
}
type ShadowDataPtr = Arc<ShadowData>;

// emacs命令调用跟gui处理并一定是同一线程，要按多线程来处理！
use lazy_static::lazy_static;
lazy_static! {
    static ref SHADOW_WNDOWS: Mutex<HashMap<usize, ShadowDataPtr>> = Mutex::new(HashMap::new());
}
static mut SHADOW_BRUSH: HBRUSH = std::ptr::null_mut();

fn set_window_pos(pthis: &ShadowData, parent: Option<HWND>) {
    let x = pthis.xpos.load(Ordering::SeqCst).into();
    let y = pthis.ypos.load(Ordering::SeqCst).into();
    let w = pthis.width.load(Ordering::SeqCst).into();
    let h = pthis.height.load(Ordering::SeqCst).into();
    unsafe {
        SetWindowPos(
            pthis.hwnd as HWND,
            HWND_TOPMOST, //std::ptr::null_mut(),//HWND_TOPMOST, //,//HWND_NOTOPMOST,//
            x,
            y,
            w,
            h,
            //SWP_SHOWWINDOW | SWP_NOACTIVATE,
            SWP_SHOWWINDOW | SWP_NOACTIVATE, // | , // SWP_NOSIZE  SWP_NOZORDER
        );
        if let Some(hp) = parent {
            SetWindowPos(
                hp,
                HWND_TOPMOST, //HWND_TOP
                0,
                0,
                0,
                0,
                SWP_SHOWWINDOW | SWP_NOSIZE | SWP_NOMOVE,
            );
        }
    }
}

fn create_shadow_window(hwd_parent: HWND) -> Option<ShadowData> {
    unsafe {
        debug_output!("create_shadow_window".to_owned());
        let cls_name = to_wstring("EmacsShadowWindow");
        let wnd_name = to_wstring("EmacsShadowWindowName");
        let m_h_wnd = CreateWindowExW(
            WS_EX_TOOLWINDOW | WS_EX_TRANSPARENT | WS_EX_LAYERED, //   WS_EX_TOPMOST  WS_EX_NOACTIVATE
            cls_name.as_ptr(),
            wnd_name.as_ptr(),
            WS_POPUPWINDOW, // WS_CHILD,//,//WS_VISIBLE,
            CW_USEDEFAULT,
            0,
            0,
            0,
            // hwd_parent,//; child不行，老是在emacs窗口上面
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            GetModuleHandleW(std::ptr::null_mut()) as HINSTANCE,
            NULL,
        );

        if m_h_wnd.is_null() {
            debug_output!(format!(
                "create_shadow_window CreateWindowExA failed! LastError:{}",
                GetLastError()
            ));
            return None;
        }

        let mut rc: RECT = std::mem::zeroed();
        GetWindowRect(hwd_parent, &mut rc as *mut _); // 最大化shadow初始化最下面有些挫位，影响不大
        // MapWindowPoints(HWND_DESKTOP, hwd_parent, &mut rc as *mut _ as *mut _, 2);
        let x = rc.left;
        let y = rc.top;
        let w = rc.right - rc.left;
        let h = rc.bottom - rc.top;
        debug_output!(format!("init rect: x:{}, y: {}, w:{}, h:{}", x, y, w, h));
        let sd = ShadowData {
            hwnd: m_h_wnd as usize,
            width: AtomicU16::new((w).try_into().unwrap_or(0)),
            height: AtomicU16::new((h).try_into().unwrap_or(0)),
            xpos: AtomicU16::new(x.try_into().unwrap_or(0)),
            ypos: AtomicU16::new(y.try_into().unwrap_or(0)),
            org_winproc: 0,
        };
        set_window_pos(&sd, Some(hwd_parent));
        redraw_window(&sd);
        return Some(sd);
    }
}

unsafe extern "system" fn cwndshadow_window_proc(
    hwnd: HWND,
    msg: u32,
    wparam: WPARAM,
    lparam: LPARAM,
) -> LRESULT {
    if msg == WM_MOUSEACTIVATE {
        return MA_NOACTIVATE as LRESULT;
    };
    if msg == WM_PAINT {
        debug_output!("child WM_PAINT".to_owned());
        if !SHADOW_BRUSH.is_null() {
            let mut ps: PAINTSTRUCT = std::mem::zeroed();
            let hdc = BeginPaint(hwnd, &mut ps as *mut _);
            FillRect(hdc, &ps.rcPaint, SHADOW_BRUSH as *mut _);
            EndPaint(hwnd, &ps);
        }
    }
    return DefWindowProcA(hwnd, msg, wparam, lparam);
}

// fn redraw(hdc: HDC, pthis: &ShadowData) {
//     unsafe {
//         if !SHADOW_BRUSH.is_null() {
//             let mut rc: RECT = std::mem::zeroed();
//             rc.left = 0;
//             rc.top = 0;
//             rc.right = pthis.width.load(Ordering::SeqCst) as i32;
//             rc.bottom = pthis.height.load(Ordering::SeqCst) as i32;
//             FillRect(hdc, &rc, SHADOW_BRUSH as *mut _);
//         }
//     }
// }
fn redraw_window(_pthis: &ShadowData) {
    debug_output!("child redraw_window".to_owned());
    return;
    
    // unsafe {
    //     let hdc = GetDC(pthis.hwnd as HWND);
    //     if !hdc.is_null() {
    //         redraw(hdc, &pthis);
    //         ReleaseDC(pthis.hwnd as HWND, hdc);
    //     }
    // }
}

extern "system" fn window_proc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    unsafe {
        let data;
        {
            let mut sw = SHADOW_WNDOWS.lock().unwrap();
            if let Some(shadow) = sw.get_mut(&(hwnd as usize)) {
                data = shadow.clone();
            } else {
                return DefWindowProcW(hwnd, msg, wparam, lparam);
            }
        }
        // debug_output!(format!("message:{}", msg));
        let pthis = data;
        if msg == WM_MOVE {
            let x = LOWORD(lparam as DWORD);
            let y = HIWORD(lparam as DWORD);
            debug_output!(format!("WM_MOVE, x:{}, y:{}", x, y));
            pthis.xpos.store(x, Ordering::SeqCst);
            pthis.ypos.store(y, Ordering::SeqCst);
            set_window_pos(&pthis, Some(hwnd));
        }
        if msg == WM_ERASEBKGND {
            // debug_output!("WM_ERASEBKGND".to_owned());
        }
        if msg == WM_SIZE {
            let w = LOWORD(lparam as DWORD);
            let h = HIWORD(lparam as DWORD);
            let mut out = format!("WM_SIZE  w:{}, h:{}", w, h);
            if SIZE_MINIMIZED != wparam {
                // 缩小时长宽都成0了，不能保存，否则restore时得不到长宽
                pthis.width.store(w, Ordering::SeqCst);
                pthis.height.store(h, Ordering::SeqCst);
                set_window_pos(&pthis, Some(hwnd));
                redraw_window(&pthis);
            }
            if SIZE_MAXIMIZED == wparam || SIZE_MINIMIZED == wparam {
                if SIZE_MINIMIZED == wparam {
                    ShowWindow(pthis.hwnd as HWND, SW_HIDE);
                    out += " SIZE_MINIMIZED";
                } else {
                    out += " SIZE_MINIMIZED";
                }
            }
            debug_output!(out);
        }
        if msg == WM_SHOWWINDOW {
            if wparam == 0 {
                debug_output!("WM_SHOWWINDOW SW_HIDE".to_owned());
                ShowWindow(pthis.hwnd as HWND, SW_HIDE);
            } else {
                debug_output!("WM_SHOWWINDOW SHOW".to_owned());
                ShowWindow(pthis.hwnd as HWND, SW_SHOWNA);
            }
        }
        if msg == WM_PAINT {
            debug_output!("parent WM_PAINT".to_owned());
            // update_shadow_window(pthis.hwnd as HWND);
            redraw_window(&pthis);
        }
        if msg == WM_EXITSIZEMOVE {
            redraw_window(&pthis);
        }
        if msg == WM_SETFOCUS {
            debug_output!("WM_SETFOCUS".to_owned());
        }
        if msg == WM_DESTROY {
            debug_output!("WM_DESTROY".to_owned());
            ShowWindow(pthis.hwnd as HWND, SW_HIDE);
            DestroyWindow(pthis.hwnd as HWND);
        }
        if msg == WM_NCDESTROY {
            debug_output!("WM_NCDESTROY".to_owned());
            let mut sw = SHADOW_WNDOWS.lock().unwrap();
            sw.remove(&(hwnd as usize));
        }

        return CallWindowProcW(
            Some(std::mem::transmute(pthis.org_winproc)),
            hwnd,
            msg,
            wparam,
            lparam,
        );
    }
}

fn hook_emacs_windproc(h: HWND) {
    unsafe {
        {
            let sw = SHADOW_WNDOWS.lock().unwrap();
            if sw.contains_key(&(h as usize)) {
                return;
            }
        }

        if let Some(mut sd) = create_shadow_window(h) {
            SetLastError(ERROR_SUCCESS);
            let result = SetWindowLongPtrW(h, GWLP_WNDPROC, window_proc as isize);
            if result == 0 && GetLastError() != ERROR_SUCCESS {
                debug_output!("SetWindowLongPtrW failed!".to_owned());
                return;
            }
            sd.org_winproc = result as usize;
            let mut sw = SHADOW_WNDOWS.lock().unwrap();
            if sw.contains_key(&(h as usize)) {
                return;
            }
            sw.insert(h as usize, Arc::new(sd));
        }
    }
}

fn get_current_process_wnd() -> Option<Vec<HWND>> {
    let mut data = EnumData {
        pid: unsafe { winapi::um::processthreadsapi::GetCurrentProcessId() },
        ret: Vec::new(),
    };
    unsafe {
        EnumWindows(Some(enum_callback), &mut data as *mut _ as LONG_PTR);
    }
    if !data.ret.is_empty() {
        Some(data.ret)
    } else {
        None
    }
}


fn set_transparent_one_frame(alpha: u8, hwnd: HWND) {
    unsafe {
        let old_style = GetWindowLongPtrW(hwnd, GWL_EXSTYLE);
        if 0 == old_style & WS_EX_LAYERED as LONG_PTR {
            SetWindowLongPtrW(hwnd, GWL_EXSTYLE, old_style | WS_EX_LAYERED as LONG_PTR);
        }
        SetLayeredWindowAttributes(
            hwnd,
            RGB(40, 44, 52), //RGB(0, 0, 0),
            alpha,
            LWA_ALPHA, //LWA_COLORKEY,// LWA_ALPHA,
        );
    }
}

fn set_background_transparent_one_frame(hwnd: HWND, r: u8, g: u8, b: u8) {
    unsafe {
        let old_style = GetWindowLongPtrW(hwnd, GWL_EXSTYLE);
        if 0 == old_style & WS_EX_LAYERED as LONG_PTR {
            SetWindowLongPtrW(hwnd, GWL_EXSTYLE, old_style | WS_EX_LAYERED as LONG_PTR);
        }
        SetLayeredWindowAttributes(hwnd, RGB(r, g, b), 0, LWA_COLORKEY);
    }
}

fn enable_shadow_windows(enable: bool, alpha: u8) {
    unsafe {
        let mut temp = Vec::new();
        {
            let sw = SHADOW_WNDOWS.lock().unwrap();
            for (_, s) in sw.iter() {
                temp.push(s.hwnd);
            }
        }
        for hwnd in temp {
            if enable {
                set_transparent_one_frame(alpha, hwnd as HWND);
                ShowWindow(hwnd as HWND, SW_SHOWNA);
            } else {
                ShowWindow(hwnd as HWND, SW_HIDE);
            }
        }
    }
}

// static mut
#[defun]
pub fn set_current_frame(alpha: u8) -> Result<()> {
    enable_shadow_windows(false, alpha);
    if let Some(hwnd) = get_current_process_wnd() {
        set_transparent_one_frame(alpha, hwnd[0]); // 第1个就是当前窗口
    }
    Ok(())
}

#[defun]
pub fn set_all_frame(alpha: u8) -> Result<()> {
    enable_shadow_windows(false, alpha);
    if let Some(hwnd) = get_current_process_wnd() {
        for h in hwnd {
            set_transparent_one_frame(alpha, h);
        }
    }
    Ok(())
}

static mut ORG_R: u8 = 0;
static mut ORG_G: u8 = 0;
static mut ORG_B: u8 = 0;

#[defun]
pub fn set_background(alpha: u8, r: u8, g: u8, b: u8) -> Result<()> {
    unsafe {
        if !SHADOW_BRUSH.is_null() {
            if ORG_R != r || ORG_G != g || ORG_B != b {
                DeleteObject(SHADOW_BRUSH as *mut _);
                SHADOW_BRUSH = CreateSolidBrush(RGB(r, g, b));
                if !SHADOW_BRUSH.is_null() {
                    ORG_R = r;
                    ORG_G = g;
                    ORG_B = b;
                }
            }
        } else {
            SHADOW_BRUSH = CreateSolidBrush(RGB(r, g, b));
        }

        if SHADOW_BRUSH.is_null() {
            debug_output!("SHADOW_BRUSH.is_null !".to_owned());
        }
        let mut enable = true;
        let emacs_windows = get_current_process_wnd();
        // 先给所有Emacs窗口设置透明颜色
        if let Some(hwnd) = &emacs_windows {
            for hp in hwnd {
                let h = *hp;
                if alpha != 255 {
                    hook_emacs_windproc(h);
                    set_background_transparent_one_frame(h, r, g, b);
                } else {
                    // 目前是TOPMOST实现的文字不透明，背景透明，因此在255不透明时去掉TOPMOST
                    remove_topmost(h);
                    set_transparent_one_frame(255, h);
                    enable = false;
                }
            }
        }
        enable_shadow_windows(enable, alpha);

        // 再把emacs置顶一下
        if alpha != 255{
            if let Some(hwnd) = &emacs_windows {
                for hp in hwnd {
                    SetWindowPos(
                        *hp,
                        HWND_TOPMOST, //HWND_TOP
                        0,
                        0,
                        0,
                        0,
                        SWP_SHOWWINDOW | SWP_NOSIZE | SWP_NOMOVE,
                    );
                }
            }    
        }
    }
    Ok(())
}

fn remove_topmost(h: HWND) {
    unsafe {
        SetWindowPos(
            h,
            HWND_NOTOPMOST, //HWND_TOP
            0,
            0,
            0,
            0,
            SWP_NOSIZE | SWP_NOMOVE,
        );
    }
}
