mod beacon;
mod gui;
mod shellmenu;
mod transparent;

// (module-load "pop_select")
// (message (pop-select/select (vector "aa" "bb" "aa" "bb""aa" "bb""aa" "bb""aa" "bb""aa" "bb""aa" "中文")))
// (global-set-key (kbd "<C-tab>") (lambda () (interactive)(pop-select/select (vector "aa" "bb" "aa" "bb""aa" "bb""aa" "bb""aa" "bb""aa" "bb""aa" "中文"))))

// 需要设置环境变量LIBCLANG_PATH E:\Program Files\LLVM\bin\
use emacs::{
    defun,
    Env,
    Result,
    Vector, // Value
};
use std::ffi::OsStr;
use std::os::windows::ffi::OsStrExt;
use winapi::um::debugapi::OutputDebugStringW;

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

#[emacs::module(separator = "/")]
fn init(_: &Env) -> Result<()> {
    gui::gui_init();
    beacon::becaon_init();
    transparent::transparent_init();
    Ok(())
}

#[defun]
fn pop_select(name: Vector, to_sel: usize) -> Result<usize> {
    let mut v = Vec::new();
    if let Ok(s) = name.size() {
        for i in 0..s {
            if let Ok(ss) = name.get(i) {
                v.push(ss);
            }
        }
        if let Some(s) = gui::popup(unsafe { DLL_MOD }, v, to_sel) {
            return Ok(s);
        }
    }
    return Ok(0);
}
static mut DLL_MOD: winapi::shared::minwindef::HINSTANCE = std::ptr::null_mut();

#[no_mangle]
#[allow(non_snake_case)]
extern "system" fn DllMain(
    hinstAcc: winapi::shared::minwindef::HINSTANCE,
    reason: u32,
    _: winapi::shared::minwindef::LPVOID,
) -> i32 {
    match reason {
        1 => unsafe {
            DLL_MOD = hinstAcc;
        },
        0 => (),
        _ => (),
    }
    1
}

fn to_wstring(s: &str) -> Vec<u16> {
    OsStr::new(s)
        .encode_wide()
        .chain(std::iter::once(0))
        .collect()
}

// 让标题和滚动条支持in10的dark mode，emacs28.1版本不支持，最新版本支持，但最新版本有点卡
// https://github.com/emacs-mirror/emacs/blob/1a9175a0de98676ac9aa1dec8f3c5c585ce2a9cd/src/w32fns.c#L11209-L11238
// https://github.com/godotengine/godot-proposals/issues/1868
#[defun]
fn ensure_all_window_dark_mode() -> Result<usize> {
    crate::gui::ensure_all_window_dark_mode();
    Ok(0)
}

#[defun]
fn popup_shell_menu(path: String, x: usize, y: usize) -> Result<usize> {
    if let Err(e) = crate::shellmenu::pop_shell_menu(path, x, y) {
        let es = format!("popup_shell_menu error: {}", e);
        let esw = to_wstring(&es);
        unsafe {
            OutputDebugStringW(esw.as_ptr());
        }
    }
    Ok(0)
}
