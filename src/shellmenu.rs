extern crate native_windows_gui as nwg;
use nwg::NwgError;
use std::cell::RefCell;
use winapi::um::ole2::OleInitialize;
use winapi::{shared::ntdef::LPCWSTR, um::winuser::*};
// 弹shell窗口，测试发现SetWindowSubclass跨线程不会成功，因为emacs module的运行线程不是gui线程。所以需要创建一个父窗口。
// 再者，按MSDN说明TrackPopupMenuEx的父窗口需要前置，否则点空白处menu不会退出，所以这里是窗口最大化，并且设置成透明1（测试完全透明不行）

extern "C" {
    fn PopupShellMenu(
        h: winapi::shared::windef::HWND,
        path: *const LPCWSTR,
        x: i32,
        y: i32,
        showExtraHead: i32,
    );
    fn CopyPathsToClipboard(path: *const LPCWSTR);
    fn CutPathsToClipboard(path: *const LPCWSTR);
    fn PasteToPathFromClipboard(path: LPCWSTR);
}

thread_local! {
    // 测试menu的透明父窗口是可以重用的
    static SHELL_PARENT_WND: RefCell<Option<nwg::Window>>  = RefCell::new(None);
}

pub fn shellmenu_init() {
    // 复制到进程外需要调用OleInitialize
    unsafe {
        OleInitialize(std::ptr::null_mut());
    }
}

pub fn pop_shell_menu(
    paths: Vec<String>,
    x: i32,
    y: i32,
    show_extra_head: i32,
) -> Result<(), NwgError> {
    SHELL_PARENT_WND.with(|wnd| {
        if (*wnd.borrow()).is_none() {
            nwg::init().ok(); // 必须，会注册ngw的类

            let mut window = Default::default();
            if nwg::Window::builder()
                .ex_flags(WS_EX_TOOLWINDOW) // 无任务栏窗口
                // .flags(nwg::WindowFlags::POPUP | nwg::WindowFlags::VISIBLE) // |
                .maximized(true)
                .build(&mut window)
                .is_ok()
            {
                crate::transparent::set_transparent_one_frame(1, window.handle.hwnd().unwrap());
                *wnd.borrow_mut() = Some(window);
            }
        }

        if let Some(window) = &*wnd.borrow() {
            unsafe {
                ShowWindow(window.handle.hwnd().unwrap(), SW_SHOW);
            }

            if let Ok(handler) =
                nwg::bind_raw_event_handler(&window.handle, 0x10000, move |hwnd, msg, _w, _l| {
                    if msg == (WM_USER + 1) {
                        unsafe {
                            let mut vp = Vec::new();
                            for p in &paths {
                                vp.push(crate::to_wstring(&p));
                            }
                            let mut vpr = Vec::new();
                            for p in &vp {
                                vpr.push(p.as_ptr());
                            }
                            vpr.push(std::ptr::null()); // 以0结尾
                            PopupShellMenu(hwnd, vpr.as_ptr(), x, y, show_extra_head);
                            ShowWindow(hwnd, SW_HIDE);
                        }
                        nwg::stop_thread_dispatch();
                    }
                    None
                })
            {
                unsafe {
                    winapi::um::winuser::PostMessageW(
                        window.handle.hwnd().unwrap(),
                        WM_USER + 1,
                        0,
                        0,
                    );
                }
                nwg::dispatch_thread_events();
                nwg::unbind_raw_event_handler(&handler).ok();
            }
        }
    });
    Ok(())
}

pub fn shell_copyfiles(paths: Vec<String>) {
    let mut vp = Vec::new();
    for p in &paths {
        vp.push(crate::to_wstring(&p));
    }
    let mut vpr = Vec::new();
    for p in &vp {
        vpr.push(p.as_ptr());
    }
    vpr.push(std::ptr::null()); // 以0结尾
    unsafe {
        CopyPathsToClipboard(vpr.as_ptr());
    }
}

pub fn shell_cutfiles(paths: Vec<String>) {
    let mut vp = Vec::new();
    for p in &paths {
        vp.push(crate::to_wstring(&p));
    }
    let mut vpr = Vec::new();
    for p in &vp {
        vpr.push(p.as_ptr());
    }
    vpr.push(std::ptr::null()); // 以0结尾
    unsafe {
        CutPathsToClipboard(vpr.as_ptr());
    }
}

pub fn shell_pastefiles(path: String) {
    let p = crate::to_wstring(&path);
    unsafe {
        PasteToPathFromClipboard(p.as_ptr());
    }
}
