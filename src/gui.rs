use emacs::{defun, Result};
use native_windows_gui::ControlHandle;
use std::cell::RefCell;
use winapi::shared::ntdef::NULL;
use winapi::um::debugapi::*;
use winapi::um::libloaderapi::*;
use winapi::um::wingdi::*;
extern crate native_windows_gui as nwg;
use std::collections::HashMap;
use std::rc::Rc;
use winapi::shared::basetsd::LONG_PTR;
use winapi::shared::minwindef::*;
use winapi::shared::windef::*;
use winapi::um::processthreadsapi::*;
use winapi::um::wingdi::RGB;
use winapi::um::winuser::*;

// hotkey参考 https://blog.csdn.net/x356982611/article/details/16341797
static mut HACCEL: usize = 0;

pub fn popup(
    h_mod: winapi::shared::minwindef::HINSTANCE,
    pop_list: Vec<String>,
    to_sel: usize,
) -> Option<usize> {
    if unsafe { HACCEL == 0 } {
        unsafe {
            HACCEL =
                winapi::um::winuser::LoadAcceleratorsA(h_mod, b"IDR_ACCELERATOR1\0".as_ptr() as _)
                    as usize;
        }

        nwg::init().ok()?;
        nwg::Font::set_global_family("Segoe UI").ok()?;
    }

    let mut window = Default::default();
    let mut list = Default::default();
    let layout = Default::default();
    const LIST_ITEM_HEIGHT: u16 = 23;
    nwg::Window::builder()
        .size((
            300,
            std::cmp::min(
                nwg::Monitor::height(),
                LIST_ITEM_HEIGHT as i32 * (pop_list.len() + 1) as i32, // 需要预留一行，不然会有滚动条
            ),
        ))
        .ex_flags(WS_EX_TOOLWINDOW) // 无任务栏窗口
        .flags(nwg::WindowFlags::POPUP | nwg::WindowFlags::VISIBLE)
        .position((300, 300))
        // .title("Basic example")
        .topmost(true)
        .center(true)
        .build(&mut window)
        .ok()?;

    nwg::ListBox::builder()
        .collection(pop_list)
        .flags(nwg::ListBoxFlags::VISIBLE | nwg::ListBoxFlags::TAB_STOP)
        // .focus(true)  // 不能设置focus，否则抓不到快捷键
        .selected_index(Some(0))
        .parent(&window)
        .build(&mut list)
        .ok()?;

    nwg::GridLayout::builder()
        .parent(&window)
        .margin([1, 1, 1, 1])
        .spacing(0)
        .child(0, 0, &list)
        // .child_item(nwg::GridLayoutItem::new(&hello_button, 0, 1, 1, 2))
        .build(&layout)
        .ok()?;

    let window = Rc::new(window);
    let list = Rc::new(list);
    let list1 = list.clone();
    list.set_selection(Some(to_sel));
    unsafe {
        SendMessageW(
            list.handle.hwnd().unwrap(),
            LB_SETITEMHEIGHT,
            0,
            MAKELONG(LIST_ITEM_HEIGHT, 0) as isize,
        );
    }

    // 总是遇到弹出的窗口失去焦点的情况，参考Findwindow那样去做
    unsafe {
        let hwnd = window.handle.hwnd().unwrap();
        let h_fore_wnd = GetForegroundWindow();
        let dw_cur_id = GetCurrentThreadId();
        let dw_fore_id = GetWindowThreadProcessId(h_fore_wnd, std::ptr::null_mut());

        AttachThreadInput(dw_cur_id, dw_fore_id, TRUE);
        ShowWindow(hwnd, SW_SHOWNORMAL);
        SetWindowPos(hwnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE);
        SetWindowPos(hwnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE);
        SetForegroundWindow(hwnd);
        SetFocus(hwnd);
        AttachThreadInput(dw_cur_id, dw_fore_id, FALSE);
    }
    // hook list也可以，但是头次ctrl+shift+tab没反应
    let handler = nwg::bind_raw_event_handler(&window.handle, 0x10000, move |_hwnd, msg, w, _l| {
        if msg == WM_COMMAND {
            let cmd = winapi::shared::minwindef::LOWORD(w as u32);
            const ID_CTRLTAB: u16 = 40001;
            const ID_CTRLTAB_SHIFT: u16 = 40002;
            const ID_CTRL_P: u16 = 40003;
            const ID_CTRL_N: u16 = 40004;
            let mut to_next = false;
            let mut to_prev = false;
            if cmd == ID_CTRLTAB || cmd == ID_CTRL_N {
                to_next = true;
            } else if ID_CTRLTAB_SHIFT == cmd || ID_CTRL_P == cmd {
                to_prev = true;
            }
            if to_next || to_prev {
                let to_sel;
                let len = list1.len();
                let cur_sel = list1.selection();
                if len != 0 {
                    if to_prev {
                        if let Some(cur) = cur_sel {
                            let mut cur = cur as isize;
                            cur -= 1;
                            if cur < 0 {
                                cur = len as isize - 1;
                            }
                            to_sel = Some(cur as usize);
                        } else {
                            to_sel = Some(len - 1);
                        }
                    } else {
                        if let Some(mut cur) = cur_sel {
                            cur += 1;
                            if cur >= len {
                                cur = 0;
                            }
                            to_sel = Some(cur);
                        } else {
                            to_sel = Some(0);
                        }
                    }
                    list1.set_selection(to_sel);
                }
            }
        } else if msg == WM_KEYUP {
            let ctrl = unsafe { GetAsyncKeyState(VK_CONTROL) as i32 & 0x8000 };
            if ctrl == 0 {
                nwg::stop_thread_dispatch();
            }
        } else if msg == WM_KILLFOCUS {
            nwg::stop_thread_dispatch();
        }
        None
    })
    .ok()?;

    // 拷贝nwg::dispatch_thread_events()代码，添加TranslateAcceleratorW
    unsafe {
        let mut msg: MSG = std::mem::zeroed();
        while GetMessageW(&mut msg, std::ptr::null_mut(), 0, 0) != 0 {
            if TranslateAcceleratorW(msg.hwnd, HACCEL as _, &mut msg) != 0 {
                continue;
            }
            if IsDialogMessageW(GetAncestor(msg.hwnd, GA_ROOT), &mut msg) == 0 {
                TranslateMessage(&msg);
                DispatchMessageW(&msg);
            }
        }
    }

    nwg::unbind_raw_event_handler(&handler).ok();
    // dbg!(&selection_string);
    // *selection_string
    list.selection()
}

pub fn gui_init() {
    // 不知道有没有效果，保留吧
    unsafe {
        let mut dwtimeout: DWORD = u32::MAX as DWORD;
        SystemParametersInfoA(
            SPI_GETFOREGROUNDLOCKTIMEOUT,
            0,
            &mut dwtimeout as *mut _ as *mut _,
            0,
        );
        if dwtimeout >= 100 {
            SystemParametersInfoA(
                SPI_SETFOREGROUNDLOCKTIMEOUT,
                0,
                std::ptr::null_mut(),
                SPIF_SENDCHANGE | SPIF_UPDATEINIFILE,
            );
        }
    }
}

// ======================== 以下实现窗口透明 ============================
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
    hwnd: HWND,
    status: ShadowStatus,
    width: WORD,
    height: WORD,
    xpos: WORD,
    ypos: WORD,
}
type ShadowDataPtr = RefCell<ShadowData>;
static mut SHADOW_WNDOWS: Option<HashMap<usize, ShadowDataPtr>> = None;
static mut EMACS_WINPROC_HOOKED: bool = false;
static mut SHADOW_BRUSH: HBRUSH = std::ptr::null_mut();
static mut IS_BACKGROUND_TRANSPARENT_MODE: bool = false;

fn find_shadow_window(hwd_parent: HWND) -> Option<&'static ShadowDataPtr> {
    unsafe {
        if let Some(hm) = &SHADOW_WNDOWS {
            return hm.get(&(hwd_parent as usize));
        } else {
            return create_shadow_window(hwd_parent);
        }
    }
}

fn create_shadow_window(hwd_parent: HWND) -> Option<&'static ShadowDataPtr> {
    unsafe {
        // 检查是否是Emacs窗口
        let mut buf: [u8; MAX_PATH] = std::mem::zeroed();
        let len = GetClassNameA(hwd_parent, &mut buf as *mut _ as *mut _, MAX_PATH as i32);
        if !(len == 5 && &buf[0..5] == b"Emacs") {
            return None;
        }
        let m_h_wnd = CreateWindowExA(
            WS_EX_LAYERED | WS_EX_TRANSPARENT,
            b"emacs_shadow\0".as_ptr() as *const _,
            std::ptr::null_mut(),
            /*WS_VISIBLE | *//*WS_CAPTION | */ WS_POPUPWINDOW,
            CW_USEDEFAULT,
            0,
            0,
            0,
            hwd_parent,
            std::ptr::null_mut(),
            GetModuleHandleW(std::ptr::null_mut()) as HINSTANCE,
            NULL,
        );
        if m_h_wnd.is_null() {
            return None;
        }
        if SHADOW_WNDOWS.is_none() {
            SHADOW_WNDOWS = Some(HashMap::new());
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
            hwnd: m_h_wnd,
            status,
            width: 0,
            height: 0,
            xpos: 0,
            ypos: 0,
        };
        if let Some(hm) = &mut SHADOW_WNDOWS {
            hm.insert(hwd_parent as usize, RefCell::new(sd));
            return hm.get(&(hwd_parent as usize));
        }
        None
    }
}
static mut gRawEventHandler: Option<nwg::RawEventHandler> = None;
fn hook_emacs_windproc(hwnd: HWND) {
    unsafe {
        if !EMACS_WINPROC_HOOKED {
            OutputDebugStringA(b"hook_emacs_windproc\0".as_ptr() as *const _);
            winapi::shared::winerror
                let ch = ControlHandle::Hwnd(hwnd);
            // TODO: 暂时不考虑释放
            let handler = nwg::bind_raw_event_handler(&ch, 0x10001, |hwnd, msg, wparam, lparam| {
                OutputDebugStringA(b"bind_raw_event_handler\0".as_ptr() as *const _);

                if let Some(shadow) = find_shadow_window(hwnd) {
                    let mut pthis = shadow.borrow_mut();

                    if msg == WM_MOVE {
                        OutputDebugStringA(b"WM_MOVE\0".as_ptr() as *const _);
                        if pthis.status.contains(ShadowStatus::SS_VISABLE) {
                            // let mut WndRect: RECT = std::mem::zeroed();
                            // GetWindowRect(hwnd, &mut WndRect as *mut _);
                            pthis.xpos = LOWORD(lparam.try_into().unwrap());
                            pthis.ypos = HIWORD(lparam.try_into().unwrap());
                            SetWindowPos(
                                pthis.hwnd,
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
                        OutputDebugStringA(b"WM_ERASEBKGND\0".as_ptr() as *const _);

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
                        OutputDebugStringA(b"WM_SIZE\0".as_ptr() as *const _);

                        if pthis.status.contains(ShadowStatus::SS_ENABLED) {
                            pthis.width = LOWORD(lparam.try_into().unwrap());
                            pthis.height = HIWORD(lparam.try_into().unwrap());
                            if SIZE_MAXIMIZED == wparam || SIZE_MINIMIZED == wparam {
                                if SIZE_MINIMIZED == wparam {
                                    ShowWindow(pthis.hwnd, SW_HIDE);
                                    pthis.status.remove(ShadowStatus::SS_VISABLE);
                                } else {
                                    SendMessageW(pthis.hwnd, WM_SIZE, SIZE_MAXIMIZED, 0);
                                }
                            } else if pthis.status.contains(ShadowStatus::SS_PARENTVISIBLE) {
                                // Parent maybe resized even if invisible
                                if !pthis.status.contains(ShadowStatus::SS_VISABLE) {
                                    ShowWindow(pthis.hwnd, SW_SHOWNA);
                                    pthis.status.insert(ShadowStatus::SS_VISABLE);
                                }
                            }
                        }
                    }
                    if msg == WM_SHOWWINDOW {
                        OutputDebugStringA(b"WM_SHOWWINDOW\0".as_ptr() as *const _);

                        if pthis.status.contains(ShadowStatus::SS_ENABLED) {
                            if wparam == 0 {
                                ShowWindow(pthis.hwnd, SW_HIDE);
                                pthis.status.remove(ShadowStatus::SS_VISABLE);
                                pthis.status.remove(ShadowStatus::SS_PARENTVISIBLE);
                            } else if !pthis.status.contains(ShadowStatus::SS_PARENTVISIBLE) {
                                ShowWindow(pthis.hwnd, SW_SHOWNA);
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
                }
                None
            });
            if handler.is_err() {
                OutputDebugStringA(b"ERROR in hook_emacs_windproc_\0".as_ptr() as *const _);
            }
            else{
                gRawEventHandler = handler.ok();
            }
            EMACS_WINPROC_HOOKED = true;
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
        hook_emacs_windproc(data.ret[0]);
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

// static mut
#[defun]
pub fn set_transparent_current_frame(alpha: u8) -> Result<()> {
    enable_shadow_windows(false, alpha);
    if let Some(hwnd) = get_current_process_wnd() {
        set_transparent_one_frame(alpha, hwnd[0]); // 第1个就是当前窗口
    }
    Ok(())
}

#[defun]
pub fn set_transparent_all_frame(alpha: u8) -> Result<()> {
    enable_shadow_windows(false, alpha);
    if let Some(hwnd) = get_current_process_wnd() {
        for h in hwnd {
            set_transparent_one_frame(alpha, h);
        }
    }
    Ok(())
}

fn enable_shadow_windows(enable: bool, alpha: u8) {
    unsafe {
        IS_BACKGROUND_TRANSPARENT_MODE = enable;
        if let Some(sw) = &SHADOW_WNDOWS {
            for (_, s) in sw.iter() {
                let s = s.borrow();
                if enable {
                    set_transparent_one_frame(alpha, s.hwnd);
                    ShowWindow(s.hwnd, SW_SHOW);
                } else {
                    ShowWindow(s.hwnd, SW_HIDE);
                }
            }
        }
    }
}

#[defun]
pub fn set_background_transparent(alpha: u8, r: u8, g: u8, b: u8) -> Result<()> {
    unsafe {
        if !SHADOW_BRUSH.is_null() {
            DeleteObject(SHADOW_BRUSH as *mut _);
        }
        SHADOW_BRUSH = CreateSolidBrush(RGB(r, g, b));
        // 先给所有Emacs窗口设置透明颜色
        if let Some(hwnd) = get_current_process_wnd() {
            for h in hwnd {
                set_background_transparent_one_frame(h, r, g, b);
            }
        }
        enable_shadow_windows(true, alpha);
    }
    Ok(())
}
