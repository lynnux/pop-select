﻿mod gui;
mod beacon;
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

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

#[emacs::module(separator = "/")]
fn init(_: &Env) -> Result<()> {
    gui::gui_init();
    beacon::becaon_init();
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
