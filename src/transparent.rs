use crate::to_wstring;
use emacs::{defun, Result};
use std::sync::Mutex;
use winapi::shared::ntdef::NULL;
use winapi::shared::winerror::ERROR_SUCCESS;
use winapi::um::debugapi::*;
use winapi::um::errhandlingapi::GetLastError;
use winapi::um::errhandlingapi::SetLastError;
use winapi::um::libloaderapi::*;
use winapi::um::wingdi::*;
extern crate native_windows_gui as nwg;
use std::collections::HashMap;
use winapi::shared::basetsd::LONG_PTR;
use winapi::shared::minwindef::*;
use winapi::shared::windef::*;
use winapi::um::processthreadsapi::*;
use winapi::um::wingdi::RGB;
use winapi::um::winuser::*;

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
                OutputDebugStringA(b"RegisterClassEx failed\0".as_ptr() as *const _);
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

use bitflags::bitflags;

bitflags! {
    struct ShadowStatus : u32 {
        const SS_ENABLED = 0b00000001;       // Shadow is enabled, if not, the following one is always false
        const SS_VISABLE = 0b00000010;       // Shadow window is visible
        const SS_PARENTVISIBLE = 0b00000100; // Parent window is visible, if not, the above one is always false
    }
}

struct ShadowData {
    hwnd: usize,
    status: ShadowStatus,
    width: WORD,
    height: WORD,
    xpos: WORD,
    ypos: WORD,
    org_winproc: usize, //WNDPROC,
}
type ShadowDataPtr = ShadowData;

// emacs命令调用跟gui处理并一定是同一线程，要按多线程来处理！
use lazy_static::lazy_static;
lazy_static! {
    static ref SHADOW_WNDOWS: Mutex<HashMap<usize, ShadowDataPtr>> = Mutex::new(HashMap::new());
}
static mut SHADOW_BRUSH: HBRUSH = std::ptr::null_mut();
static mut IS_BACKGROUND_TRANSPARENT_MODE: bool = false;

fn create_shadow_window(hwd_parent: HWND) -> Option<ShadowData> {
    unsafe {
        debug_output("create_shadow_window".to_owned());
        let cls_name = to_wstring("EmacsShadowWindow");
        let wnd_name = to_wstring("EmacsShadowWindow");
        let m_h_wnd = CreateWindowExW(
            WS_EX_LAYERED | WS_EX_TRANSPARENT,
            cls_name.as_ptr(),
            wnd_name.as_ptr(),
            /*WS_VISIBLE | *//*WS_CAPTION | */ WS_POPUPWINDOW,
            CW_USEDEFAULT,
            0,
            0,
            0,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            GetModuleHandleW(std::ptr::null_mut()) as HINSTANCE,
            NULL,
        );

        if m_h_wnd.is_null() {
            debug_output(format!(
                "create_shadow_window CreateWindowExA failed! LastError:{}",
                GetLastError()
            ));
            return None;
        }

        let parent_style = GetWindowLongA(hwd_parent, GWL_STYLE);
        let status: ShadowStatus;
        if (WS_VISIBLE & parent_style as u32) != 0 {
            status = ShadowStatus::SS_ENABLED;
        } else if ((WS_MAXIMIZE | WS_MINIMIZE) & parent_style as u32) != 0 {
            status = ShadowStatus::SS_ENABLED | ShadowStatus::SS_PARENTVISIBLE;
        } else {
            status = ShadowStatus::SS_ENABLED
                | ShadowStatus::SS_VISABLE
                | ShadowStatus::SS_PARENTVISIBLE;
            ShowWindow(m_h_wnd, SW_SHOWNA);
            // Update(hParentWnd);
        }
        let sd = ShadowData {
            hwnd: m_h_wnd as usize,
            status,
            width: 0,
            height: 0,
            xpos: 0,
            ypos: 0,
            org_winproc: 0,
        };
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
    return DefWindowProcA(hwnd, msg, wparam, lparam);
}

extern "system" fn window_proc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    unsafe {
        let mut sw = SHADOW_WNDOWS.lock().unwrap();
        if let Some(mut shadow) = sw.get_mut(&(hwnd as usize)) {
            let mut pthis = &mut shadow;
            // debug_output(format!("hwnd:{}, msg:{}, wparam:{}, lparam:{}", hwnd as usize, msg, wparam, lparam));
            if msg == WM_MOVE {
                debug_output("WM_MOVE".to_owned());
                if pthis.status.contains(ShadowStatus::SS_VISABLE) {
                    // let mut WndRect: RECT = std::mem::zeroed();
                    // GetWindowRect(hwnd, &mut WndRect as *mut _);
                    pthis.xpos = LOWORD(lparam as DWORD);
                    pthis.ypos = HIWORD(lparam as DWORD);
                    SetWindowPos(
                        pthis.hwnd as HWND,
                        std::ptr::null_mut(),
                        pthis.xpos.into(),
                        pthis.ypos.into(),
                        pthis.width.into(),
                        pthis.height.into(),
                        SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER,
                    );
                }
            }
            if msg == WM_ERASEBKGND {
                debug_output("WM_ERASEBKGND".to_owned());

                // 画背景色
                let hdc = wparam as HDC;
                if !hdc.is_null() && !SHADOW_BRUSH.is_null() {
                    let mut rc: RECT = std::mem::zeroed();
                    rc.left = 0;
                    rc.top = 0;
                    rc.right = pthis.width as i32;
                    rc.bottom = pthis.height as i32;
                    FillRect(hdc, &rc, SHADOW_BRUSH as *mut _);
                }
            }
            if msg == WM_SIZE {
                debug_output("WM_SIZE".to_owned());

                if pthis.status.contains(ShadowStatus::SS_ENABLED) {
                    pthis.width = LOWORD(lparam as DWORD);
                    pthis.height = HIWORD(lparam as DWORD);
                    if SIZE_MAXIMIZED == wparam || SIZE_MINIMIZED == wparam {
                        if SIZE_MINIMIZED == wparam {
                            ShowWindow(pthis.hwnd as HWND, SW_HIDE);
                            pthis.status.remove(ShadowStatus::SS_VISABLE);
                        } else {
                            SendMessageW(pthis.hwnd as HWND, WM_SIZE, SIZE_MAXIMIZED, 0);
                        }
                    } else if pthis.status.contains(ShadowStatus::SS_PARENTVISIBLE) {
                        // Parent maybe resized even if invisible
                        if !pthis.status.contains(ShadowStatus::SS_VISABLE) {
                            ShowWindow(pthis.hwnd as HWND, SW_SHOWNA);
                            pthis.status.insert(ShadowStatus::SS_VISABLE);
                        }
                    }
                }
            }
            if msg == WM_SHOWWINDOW {
                debug_output("WM_SHOWWINDOW".to_owned());

                if pthis.status.contains(ShadowStatus::SS_ENABLED) {
                    if wparam == 0 {
                        ShowWindow(pthis.hwnd as HWND, SW_HIDE);
                        pthis.status.remove(ShadowStatus::SS_VISABLE);
                        pthis.status.remove(ShadowStatus::SS_PARENTVISIBLE);
                    } else if !pthis.status.contains(ShadowStatus::SS_PARENTVISIBLE) {
                        ShowWindow(pthis.hwnd as HWND, SW_SHOWNA);
                        pthis.status.insert(ShadowStatus::SS_VISABLE);
                        pthis.status.insert(ShadowStatus::SS_PARENTVISIBLE);
                    }
                }
            }
            // TODO: 暂时不做删除，也不会有很多窗口
            // if msg == WM_DESTROY{
            //     DestroyWindow(pthis.hwnd);
            // }
            // if msg == WM_NCDESTROY{
            // }

            return CallWindowProcW(
                Some(std::mem::transmute(pthis.org_winproc)),
                hwnd,
                msg,
                wparam,
                lparam,
            );
        } else {
            debug_output("window_proc failed2".to_owned());
        }

        return DefWindowProcW(hwnd, msg, wparam, lparam);
    }
}

fn hook_emacs_windproc(h: HWND) {
    debug_output("hook_emacs_windproc".to_owned());
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
                debug_output("SetWindowLongPtrW failed!".to_owned());
                return;
            }
            sd.org_winproc = result as usize;
            // struct ShadowData {
            //     hwnd: HWND,
            //     status: ShadowStatus,
            //     width: WORD,
            //     height: WORD,
            //     xpos: WORD,
            //     ypos: WORD,
            //     org_winproc: WNDPROC,
            // }
            // debug_output(format!("shadow hwnd:{}, org_winproc:{}", sb.hwnd as usize, sb.org_winproc.unwrap() as usize));
            let mut sw = SHADOW_WNDOWS.lock().unwrap();
            if sw.contains_key(&(h as usize)) {
                return;
            }
            sw.insert(h as usize, sd);
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

fn debug_output(mut str: String) {
    unsafe {
        str.insert_str(
            0,
            &format!(
                "pid:{}, tid:{} ",
                GetCurrentProcessId(),
                GetCurrentThreadId()
            ),
        );
        let s = to_wstring(&str);
        OutputDebugStringW(s.as_ptr() as *const _);
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
        IS_BACKGROUND_TRANSPARENT_MODE = enable;
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
                ShowWindow(hwnd as HWND, SW_SHOW);
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

#[defun]
pub fn set_background(alpha: u8, r: u8, g: u8, b: u8) -> Result<()> {
    unsafe {
        if !SHADOW_BRUSH.is_null() {
            DeleteObject(SHADOW_BRUSH as *mut _);
        }
        SHADOW_BRUSH = CreateSolidBrush(RGB(r, g, b));
        // 先给所有Emacs窗口设置透明颜色
        if let Some(hwnd) = get_current_process_wnd() {
            for h in hwnd {
                hook_emacs_windproc(h);
                set_background_transparent_one_frame(h, r, g, b);
            }
        }
        enable_shadow_windows(true, alpha);
    }
    Ok(())
}
