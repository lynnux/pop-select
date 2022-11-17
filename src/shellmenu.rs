extern crate native_windows_gui as nwg;
use nwg::NwgError;
use winapi::{shared::ntdef::LPCWSTR, um::{winuser::*}};
use winapi::um::ole2::OleInitialize;

extern "C" {
    fn PopupShellMenu(h: winapi::shared::windef::HWND, path: *const LPCWSTR, x: i32, y: i32);
}

pub fn shellmenu_init(){
    // 复制到进程外需要调用OleInitialize
    unsafe{OleInitialize(std::ptr::null_mut());}
}

pub fn pop_shell_menu(paths: Vec<String>, x: i32, y: i32) -> Result<(), NwgError> {
    // use winapi::um::winuser::GetForegroundWindow;
    // let h = GetForegroundWindow();
    // 测试发现SetWindowSubclass跨线程不会成功，因为emacs module的运行线程不是gui线程。所以这里需要跟ctrl+tab那样的处理机制。
    
    nwg::init()?; // 必须，会注册ngw的类

    let mut window = Default::default();
    nwg::Window::builder()
        .ex_flags(WS_EX_TOOLWINDOW) // 无任务栏窗口
        // .flags(nwg::WindowFlags::POPUP | nwg::WindowFlags::VISIBLE) // |
        .maximized(true)
        .build(&mut window)?;
    // 按MSDN说明TrackPopupMenuEx的父窗口需要前置，否则点空白处menu不会退出，所以这里是窗口最大化，并且设置成透明1（测试完全透明不行） ，效果很好！
    crate::transparent::set_transparent_one_frame(1, window.handle.hwnd().unwrap());

    let handler =
        nwg::bind_raw_event_handler(&window.handle, 0x10000, move |hwnd, msg, _w, _l| {
            if msg == (WM_USER + 1) {
                unsafe {
                    let mut vp = Vec::new();
                    for p in &paths {
                        vp.push(crate::to_wstring(&p));
                    }
                    let mut vpr = Vec::new();
                    for p in &vp{
                        vpr.push(p.as_ptr());
                    }
                    vpr.push(std::ptr::null()); // 以0结尾
                    PopupShellMenu(hwnd, vpr.as_ptr(), x, y);
                }
                nwg::stop_thread_dispatch();
            }
            None
        })?;
    unsafe {
        winapi::um::winuser::PostMessageW(window.handle.hwnd().unwrap(), WM_USER + 1, 0, 0);
    }
    nwg::dispatch_thread_events();
    nwg::unbind_raw_event_handler(&handler)?;
    Ok(())
}
