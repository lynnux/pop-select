use emacs::{defun, Result};
use winapi::ctypes::*;
use winapi::shared::basetsd::*;
use winapi::shared::minwindef::*;
use winapi::shared::windef::*;
#[cfg(debug_assertions)]
use winapi::um::debugapi::*;
#[cfg(debug_assertions)]
use winapi::um::errhandlingapi::*;
use winapi::um::libloaderapi::*;
use winapi::um::processthreadsapi::*;
use winapi::um::wingdi::*;
use winapi::um::winuser::*;

static mut BEACON_WND: HWND = std::ptr::null_mut();
static mut BEACON_TID: DWORD = 0;
static mut BEACON_WIDTH: DWORD = 300;
static mut BEACON_HEIGHT: DWORD = 20;
static mut BEACON_R: u8 = 255;
static mut BEACON_G: u8 = 0;
static mut BEACON_B: u8 = 0;
static mut BEACON_X: usize = 0;
static mut BEACON_Y: usize = 0;
static mut BEACON_DURATION: usize = 0;
static mut BEACON_DURATION_COUNT: isize = 0; //  百分比
static mut TIMER_DURATION_EACH: usize = 100;
const WM_SHOW_BEACON: UINT = WM_USER + 0x0001;
const WM_BEACON_SET_SIZE: UINT = WM_USER + 0x0002;
const TIMER_DURATION: UINT_PTR = 1;
const TIMER_DELAY: UINT_PTR = 2;

use std::ffi::OsStr;
use std::os::windows::ffi::OsStrExt;

fn to_wstring(s: &str) -> Vec<u16> {
    OsStr::new(s)
        .encode_wide()
        .chain(std::iter::once(0))
        .collect()
}

pub unsafe extern "system" fn window_proc(
    hwnd: HWND,
    msg: UINT,
    wparam: WPARAM,
    lparam: LPARAM,
) -> LRESULT {
    if msg == WM_DESTROY {
        // PostQuitMessage(0);
        return 0;
    }

    return DefWindowProcW(hwnd, msg, wparam, lparam);
}
fn on_timer(left: isize) {
    unsafe {
        let left = (left as f64 / (BEACON_DURATION as f64 / TIMER_DURATION_EACH as f64)
            * BEACON_WIDTH as f64) as i32;
        #[cfg(debug_assertions)]
        {
            let right = BEACON_WIDTH as i32 - left;
            let msg = format!("left:{}, right:{}", left, right);
            OutputDebugStringW(to_wstring(&msg).as_slice().as_ptr());
        }

        let mut vertex: [TRIVERTEX; 2] = std::mem::zeroed();
        vertex[0].x = 0;
        vertex[0].y = 0;
        vertex[0].Red = (BEACON_R as u16) << 8;
        vertex[0].Green = (BEACON_G as u16) << 8;
        vertex[0].Blue = (BEACON_B as u16) << 8;
        vertex[0].Alpha = 0x0000;

        vertex[1].x = left as i32;
        vertex[1].y = BEACON_HEIGHT as i32;
        vertex[1].Red = 0;
        vertex[1].Green = 0;
        vertex[1].Blue = 0;
        vertex[1].Alpha = 0x0000;

        // Create a GRADIENT_RECT structure that
        // references the TRIVERTEX vertices.
        let mut g_rect: GRADIENT_RECT = std::mem::zeroed();
        g_rect.UpperLeft = 0;
        g_rect.LowerRight = 1;
        let hdc = GetDC(BEACON_WND);
        if !hdc.is_null() {
            GradientFill(
                hdc,
                &mut vertex as *mut _,
                2,
                &mut g_rect as *mut _ as *mut _,
                1,
                GRADIENT_FILL_RECT_H,
            );
            let bb = GetStockObject(BLACK_BRUSH as i32);
            let mut rc: RECT = std::mem::zeroed();
            rc.left = left;
            rc.top = 0;
            rc.right = BEACON_WIDTH as i32;
            rc.bottom = BEACON_HEIGHT as i32;
            FillRect(hdc, &rc, bb as *mut _);
            ReleaseDC(BEACON_WND, hdc);
        }
    }
}
fn show_wnd() {
    unsafe {
        if BEACON_WND.is_null() {
            return;
        }
        #[cfg(debug_assertions)]
        {
            OutputDebugStringA(b"WM_SHOW_BEACON!\0" as *const _ as *const _);
        }
        ShowWindow(BEACON_WND, SW_SHOW);

        BEACON_DURATION_COUNT = (BEACON_DURATION as f64 / TIMER_DURATION_EACH as f64) as isize;
        SetTimer(BEACON_WND, TIMER_DURATION, TIMER_DURATION_EACH as u32, None);
        // SetForegroundWindow(BEACON_WND); // 这个会发生焦点切换
        SetWindowPos(
            BEACON_WND,
            HWND_TOPMOST,
            BEACON_X as c_int,
            BEACON_Y as c_int,
            BEACON_WIDTH as c_int,
            BEACON_HEIGHT as c_int,
            SWP_SHOWWINDOW,
        );
        on_timer(BEACON_DURATION_COUNT);
    }
}

fn create_wnd() {
    unsafe {
        if !BEACON_WND.is_null() {
            return;
        }
        let wc = WNDCLASSEXW {
            cbSize: std::mem::size_of::<WNDCLASSEXW>() as UINT,
            style: CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS,
            lpfnWndProc: Some(window_proc),
            cbClsExtra: 0,
            cbWndExtra: 0,
            hInstance: GetModuleHandleW(std::ptr::null_mut()) as HINSTANCE,
            hIcon: std::ptr::null_mut(),
            hCursor: LoadCursorW(std::ptr::null_mut(), IDC_ARROW),
            hbrBackground: GetStockObject(WHITE_BRUSH as i32) as HBRUSH,
            lpszMenuName: std::ptr::null_mut(),
            lpszClassName: to_wstring("rust_window_class").as_ptr(),
            hIconSm: std::ptr::null_mut(),
        };
        if RegisterClassExW(&wc) == 0 {
            #[cfg(debug_assertions)]
            {
                OutputDebugStringA(b"RegisterClassEx failed\0".as_ptr() as *const _);
            }
        }

        // 透明要带WS_EX_LAYERED
        let hwnd = CreateWindowExW(
            WS_EX_TOOLWINDOW | WS_EX_TOPMOST | WS_EX_NOACTIVATE | WS_EX_LAYERED, // 任务栏无标题，显示时无焦点切换
            wc.lpszClassName,
            to_wstring("Rust Window").as_ptr(),
            WS_POPUP, // 调试用| WS_BORDER
            0,
            0,
            BEACON_WIDTH as i32,
            BEACON_HEIGHT as i32,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            wc.hInstance,
            std::ptr::null_mut(),
        );
        if hwnd == std::ptr::null_mut() {
            #[cfg(debug_assertions)]
            {
                let err = GetLastError();
                let msg = format!("err:{}", err);
                OutputDebugStringW(to_wstring(&msg).as_slice().as_ptr());
            }
            return;
        }
        // 指定透明色
        SetLayeredWindowAttributes(
            hwnd,
            RGB(0, 0, 0),
            200,                // LWA_COLORKEY这个就没效果，是全透明的
            LWA_COLORKEY //
            // LWA_ALPHA,
        );
        BEACON_WND = hwnd;
    }
}

pub fn becaon_init() {
    std::thread::spawn(move || unsafe {
        BEACON_TID = GetCurrentThreadId();
        let mut msg = MSG {
            hwnd: std::ptr::null_mut(),
            message: 0,
            wParam: 0,
            lParam: 0,
            time: 0,
            pt: POINT { x: 0, y: 0 },
        };
        loop {
            let res = GetMessageW(&mut msg, std::ptr::null_mut(), 0, 0);
            if res == 0 || res == -1 {
                break;
            }
            TranslateMessage(&msg);
            // 消息不能去window_proc里处理
            if msg.message == WM_SHOW_BEACON {
                create_wnd();
                if !BEACON_WND.is_null() {
                    KillTimer(BEACON_WND, TIMER_DURATION);
                    SetTimer(BEACON_WND, TIMER_DELAY, msg.wParam as u32, None);
                } else {
                    #[cfg(debug_assertions)]
                    {
                        OutputDebugStringA(b"window is null!\0".as_ptr() as *const _);
                    }
                }
            } else if msg.message == WM_BEACON_SET_SIZE {
                BEACON_WIDTH = msg.wParam as DWORD;
                BEACON_HEIGHT = msg.lParam as DWORD;
                if !BEACON_WND.is_null() {
                    DestroyWindow(BEACON_WND);
                    BEACON_WND = std::ptr::null_mut();
                }
            } else if msg.message == WM_TIMER {
                if msg.wParam == TIMER_DURATION {
                    BEACON_DURATION_COUNT = BEACON_DURATION_COUNT - 1;
                    if BEACON_DURATION_COUNT < 0 {
                        #[cfg(debug_assertions)]
                        {
                            OutputDebugStringA(b"kill timer!\0" as *const _ as *const _);
                        }
                        ShowWindow(BEACON_WND, SW_HIDE);
                        KillTimer(BEACON_WND, TIMER_DURATION);
                    } else {
                        on_timer(BEACON_DURATION_COUNT);
                    }
                } else if msg.wParam == TIMER_DELAY {
                    KillTimer(BEACON_WND, TIMER_DELAY);
                    show_wnd();
                }
            }
            DispatchMessageW(&msg);
        }
    });
}

#[defun]
fn set_parameters(
    width: usize,
    height: usize,
    r: u8,
    g: u8,
    b: u8,
    duration_step: usize,
) -> Result<()> {
    unsafe {
        PostThreadMessageW(
            BEACON_TID,
            WM_BEACON_SET_SIZE,
            width as WPARAM,
            height as LPARAM,
        );
        BEACON_R = r;
        BEACON_G = g;
        BEACON_B = b;
        TIMER_DURATION_EACH = duration_step;
    }
    Ok(())
}

#[defun]
fn blink(x: usize, y: usize, timer: usize, delay: usize) -> Result<()> {
    unsafe {
        BEACON_X = x;
        BEACON_Y = y;
        BEACON_DURATION = timer;
        PostThreadMessageW(BEACON_TID, WM_SHOW_BEACON, delay as WPARAM, 0 as LPARAM);
    }
    Ok(())
}
