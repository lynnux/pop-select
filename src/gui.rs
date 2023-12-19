#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

extern crate native_windows_gui as nwg;
use crate::to_wstring;
use crate::transparent::debug_output;
use std::ffi;
use std::os::raw::c_int;
use std::rc::Rc;
use winapi::shared::basetsd::LONG_PTR;
use winapi::shared::minwindef::BOOL;
use winapi::shared::minwindef::*;
use winapi::shared::ntdef::LPCSTR;
use winapi::shared::windef::*;
use winapi::um::libloaderapi::*;
use winapi::um::processthreadsapi::*;
use winapi::um::sysinfoapi::GetVersionExW;
use winapi::um::winbase::lstrcmpiA;
use winapi::um::winnt::LPCWSTR;
use winapi::um::winuser::*;
use winapi::{shared::ntdef::HRESULT, shared::windef::HWND};

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
        BringWindowToTop(hwnd);
        AttachThreadInput(dw_cur_id, dw_fore_id, FALSE);
    }
    // hook list也可以，但是头次ctrl+shift+tab没反应
    let handler = nwg::bind_raw_event_handler(&window.handle, 0x10000, move |_hwnd, msg, w, _l| {
        if msg == WM_COMMAND {
            let cmd = winapi::shared::minwindef::LOWORD(w as u32);
            const ID_ALT_1: u16 = 40000;
            const ID_CTRLTAB: u16 = 40001;
            const ID_CTRLTAB_SHIFT: u16 = 40002;
            const ID_CTRL_P: u16 = 40003;
            const ID_CTRL_N: u16 = 40004;
            let mut to_next = false;
            let mut to_prev = false;
            if cmd == ID_CTRLTAB || cmd == ID_CTRL_N || cmd == ID_ALT_1 {
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

///////////// 实现win10 dark mode
type DwmSetWindowAttribute = unsafe extern "system" fn(
    h_wnd: HWND,
    dw_attribute: DWORD,
    pv_attribute: LPCVOID,
    cb_attribute: DWORD,
) -> HRESULT;
type SetWindowTheme =
    unsafe extern "system" fn(hwnd: HWND, pszSubAppName: LPCWSTR, pszSubIdList: LPCWSTR) -> HRESULT;

static mut DwmSetWindowAttribute_fn: usize = 0;
static mut SetWindowTheme_fn: usize = 0;
static mut w32_darkmode: Option<BOOL> = None;
static mut w32_build_number: DWORD = 0;
fn w32_applytheme(hwnd: HWND) {
    unsafe {
        if let Some(wd) = &w32_darkmode {
            if SetWindowTheme_fn != 0 {
                let pfnSetWindowTheme: SetWindowTheme = std::mem::transmute(SetWindowTheme_fn);
                let DARK_MODE_APP_NAME = to_wstring("DarkMode_Explorer");
                pfnSetWindowTheme(hwnd, DARK_MODE_APP_NAME.as_ptr(), std::ptr::null_mut());
            }
            if DwmSetWindowAttribute_fn != 0 {
                let pfnDwmSetWindowAttribute: DwmSetWindowAttribute =
                    std::mem::transmute(DwmSetWindowAttribute_fn);
                let mut attr = 20; // DWMWA_USE_IMMERSIVE_DARK_MODE;
                if w32_build_number < 19041 {
                    attr = 19; // DWMWA_USE_IMMERSIVE_DARK_MODE_OLD
                }
                pfnDwmSetWindowAttribute(hwnd, attr, &wd as *const _ as *const _, 4 as u32);
                debug_output!(format!("hwnd:{:#X}", hwnd as usize));
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
            if (len == 5 && &buf[0..5] == b"Emacs") || (len == 9 && &buf[0..9] == b"ScrollBar")
            // 实际也就这两种需要，https://github.com/emacs-mirror/emacs/blob/master/src/w32fns.c 搜索 w32_applytheme
            {
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
fn get_current_process_wnd() -> Option<Vec<HWND>> {
    let mut data = EnumData {
        pid: unsafe { winapi::um::processthreadsapi::GetCurrentProcessId() },
        ret: Vec::new(),
    };
    unsafe {
        EnumWindows(Some(enum_callback), &mut data as *mut _ as LONG_PTR);
    }

    // scrollbar属于子窗口，还要枚举字窗口
    let wd = data.ret.clone();
    for w in wd {
        unsafe {
            EnumChildWindows(w, Some(enum_callback), &mut data as *mut _ as LONG_PTR);
        }
    }
    if !data.ret.is_empty() {
        Some(data.ret)
    } else {
        None
    }
}

pub fn ensure_all_window_dark_mode() {
    unsafe {
        if w32_darkmode.is_none() {
            use winapi::um::winnt::OSVERSIONINFOW;
            let mut osi: OSVERSIONINFOW = std::mem::zeroed();
            osi.dwOSVersionInfoSize = std::mem::size_of::<OSVERSIONINFOW>() as u32;
            GetVersionExW(&mut osi as *mut _ as *mut _);
            w32_build_number = osi.dwBuildNumber; // maj获取win10可能有问题，但buildnumber一般不会错

            let dwmapi_lib = LoadLibraryA(b"dwmapi.dll\0".as_ptr() as *const _);
            if !dwmapi_lib.is_null() {
                DwmSetWindowAttribute_fn =
                    GetProcAddress(dwmapi_lib, b"DwmSetWindowAttribute\0".as_ptr() as *const _)
                        as usize;
            }
            let uxtheme_lib = LoadLibraryA(b"uxtheme.dll\0".as_ptr() as *const _);
            if !uxtheme_lib.is_null() {
                SetWindowTheme_fn =
                    GetProcAddress(uxtheme_lib, b"SetWindowTheme\0".as_ptr() as *const _) as usize;
            }

            if SetWindowTheme_fn == 0 || DwmSetWindowAttribute_fn == 0 {
                w32_darkmode = Some(0);
            } else {
                w32_darkmode = Some(1); // TODO; 参考emacs源码可以读注册表判断是否开启dark mode
            }
            debug_output!(format!("current maj:{}, build:{}, DwmSetWindowAttribute_fn: {:#X}, SetWindowTheme_fn: {:#X}", osi.dwMajorVersion, osi.dwBuildNumber, DwmSetWindowAttribute_fn, SetWindowTheme_fn));

            // hook emacs，让后面创建的窗口也有效果
            hook_CreateWindowExA();
        }
        if *w32_darkmode.as_ref().unwrap_or(&0) == 0 {
            return;
        }
    }

    if let Some(windows) = get_current_process_wnd() {
        for w in windows {
            w32_applytheme(w);
            // TODO: 目前主窗口不会即时生效，试了很多方法不能让它重绘，只能elisp里放大重绘了
        }
    }
}

// emacs调用的是CreateWindow，但x64dbg里没找到？CreateWindowExA是调用了的。rust的hook库有个跨平台的detour只支持nightly
// 定义在这里找https://github.com/microsoft/Detours/blob/24357c6a5a6bb9025a71050e50b38dbe9c02713a/src/detours.h
// demo https://github.com/DianaNites/detours/blob/master/detours-sys/src/lib.rs
type FnCreateWindowExA = unsafe extern "system" fn(
    dwExStyle: DWORD,
    lpClassName: LPCSTR,
    lpWindowName: LPCSTR,
    dwStyle: DWORD,
    x: c_int,
    y: c_int,
    nWidth: c_int,
    nHeight: c_int,
    hWndParent: HWND,
    hMenu: HMENU,
    hInstance: HINSTANCE,
    lpParam: LPVOID,
) -> HWND;

unsafe extern "system" fn CreateWindowExA_detour(
    dwExStyle: DWORD,
    lpClassName: LPCSTR,
    lpWindowName: LPCSTR,
    dwStyle: DWORD,
    x: c_int,
    y: c_int,
    nWidth: c_int,
    nHeight: c_int,
    hWndParent: HWND,
    hMenu: HMENU,
    hInstance: HINSTANCE,
    lpParam: LPVOID,
) -> HWND {
    let org: FnCreateWindowExA = std::mem::transmute(CreateWindowExAOrg);
    let ret = org(
        dwExStyle,
        lpClassName,
        lpWindowName,
        dwStyle,
        x,
        y,
        nWidth,
        nHeight,
        hWndParent,
        hMenu,
        hInstance,
        lpParam,
    );
    if !ret.is_null() {
        // lpClassName可能是atom
        if std::mem::size_of::<LPCSTR>() == 8 {
            // 64位，判断高位DWORD是否是0，是的话再判断低DWORD的HIWORD是否是0，是0那就是atom
            if (lpClassName as u64 >> 32) & 0xffffffff == 0 {
                if winapi::shared::minwindef::HIWORD(lpClassName as DWORD) == 0 {
                    return ret;
                }
            }
        } else {
            // 32位判断HIWORD是否是0，是0那就是atom
            if winapi::shared::minwindef::HIWORD(lpClassName as DWORD) == 0 {
                return ret;
            }
        }

        if 0 == lstrcmpiA(lpClassName, b"emacs\0".as_ptr() as _)
            || 0 == lstrcmpiA(lpClassName, b"ScrollBar\0".as_ptr() as _)
        {
            //debug_output!(format!("hook {:#x}", ret as usize));
            w32_applytheme(ret);
        }
    }

    ret
}

fn get_module_symbol_address(module: &str, symbol: &str) -> Option<usize> {
    let module = module
        .encode_utf16()
        .chain(std::iter::once(0))
        .collect::<Vec<u16>>();
    let symbol = std::ffi::CString::new(symbol).unwrap();
    unsafe {
        let handle = GetModuleHandleW(module.as_ptr());
        match GetProcAddress(handle, symbol.as_ptr()) as usize {
            0 => None,
            n => Some(n),
        }
    }
}

static mut CreateWindowExAOrg: usize = 0;

fn hook_CreateWindowExA() {
    if let Some(address) = get_module_symbol_address("user32.dll", "CreateWindowExA") {
        //let target: FnCreateWindowExA = ;
        unsafe {
            CreateWindowExAOrg = std::mem::transmute(address);
            let tru = &mut CreateWindowExAOrg as *mut _ as *mut *mut ffi::c_void;
            let new = CreateWindowExA_detour as *mut ffi::c_void;

            use detours_sys::{
                DetourAttach, DetourTransactionBegin, DetourTransactionCommit, DetourUpdateThread,
            };
            DetourTransactionBegin();
            DetourUpdateThread(GetCurrentThread() as _);
            DetourAttach(tru, new);
            DetourTransactionCommit();
            // Initialize AND enable the detour (the 2nd parameter can also be a closure)
        }
    } else {
        debug_output!(format!("can't get CreateWindowExA!"));
    }
}
