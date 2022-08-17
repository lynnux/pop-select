extern crate native_windows_gui as nwg;
use std::rc::Rc;
use winapi::shared::minwindef::*;
use winapi::um::processthreadsapi::*;
use winapi::um::winuser::*;
use crate::transparent::debug_output;
use crate::to_wstring;

// hotkey参考 https://blog.csdn.net/x356982611/article/details/16341797
static mut HACCEL: usize = 0;

pub fn popup(
    h_mod: winapi::shared::minwindef::HINSTANCE,
    pop_list: Vec<String>,
    to_sel: usize,
) -> Option<usize> {
    // 确认不是emacs gui线程，所以头次运行经常会失去焦点
    // debug_output!(format!("popup tid: {}", winapi::um::processthreadsapi::GetCurrentThreadId()));
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

