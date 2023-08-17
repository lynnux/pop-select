#![allow(non_upper_case_globals)]
use crate::to_wstring;
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

static mut UI_THREAD_ID: DWORD = 0;

static mut BEACON_WND: HWND = std::ptr::null_mut();
static mut BEACON_WIDTH: DWORD = 300;
static mut BEACON_HEIGHT: DWORD = 20;
static mut BEACON_R: u8 = 255;
static mut BEACON_G: u8 = 0;
static mut BEACON_B: u8 = 0;
static mut BEACON_ARG_X: usize = 0;
static mut BEACON_ARG_Y: usize = 0;
static mut BEACON_ARG_DURATION: usize = 0;
static mut BEACON_DURATION_LEFT_COUNT: isize = 0; //  百分比
static mut TIMER_BEACON_DURATION_STEP: usize = 100;
const WM_SHOW_BEACON: UINT = WM_USER + 0x0001;
const WM_BEACON_SET_SIZE: UINT = WM_USER + 0x0002;
const TIMER_BEACON_DURATION: UINT_PTR = 1;
const TIMER_BEACON_DELAY: UINT_PTR = 2;

// cursor animation，参考https://github.com/manateelazycat/holo-layer/blob/master/plugin/cursor_animation.py
const WM_SHOW_ANIMATION: UINT = WM_USER + 0x0003;
static mut ANIMATION_WND: HWND = std::ptr::null_mut();
static mut ANIMATION_ARG_DURATION: usize = 0;
static mut ANIMATION_ARG_DURATION_STEP: usize = 100;

const TIMER_ANIMATION_DURATION: UINT_PTR = 3;
static mut ANIMATION_DURATION_LEFT_COUNT: isize = 0; //  百分比
static mut cursor_color: HBRUSH = std::ptr::null_mut();
static mut cursor_color_r: u8 = 255;
static mut cursor_color_g: u8 = 255;
static mut cursor_color_b: u8 = 255;
static mut cursor_info_x: usize = 0;
static mut cursor_info_y: usize = 0;
static mut cursor_info_w: usize = 0;
static mut cursor_info_h: usize = 0;

static mut cursor_info_prev_x: usize = 0;
static mut cursor_info_prev_y: usize = 0;
static mut cursor_info_prev_w: usize = 0;
static mut cursor_info_prev_h: usize = 0;
static mut cursor_animation_percent: f32 = 1.0;

static mut cursor_start_x: usize = 0;
static mut cursor_start_y: usize = 0;
static mut cursor_end_x: usize = 0;
static mut cursor_end_y: usize = 0;

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
fn on_beacon_timer(left: isize) {
    unsafe {
        let left = (left as f64 / (BEACON_ARG_DURATION as f64 / TIMER_BEACON_DURATION_STEP as f64)
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

fn show_beacon_wnd() {
    unsafe {
        if BEACON_WND.is_null() {
            return;
        }
        #[cfg(debug_assertions)]
        {
            OutputDebugStringA(b"WM_SHOW_BEACON!\0" as *const _ as *const _);
        }
        ShowWindow(BEACON_WND, SW_SHOW);

        BEACON_DURATION_LEFT_COUNT =
            (BEACON_ARG_DURATION as f64 / TIMER_BEACON_DURATION_STEP as f64) as isize;
        SetTimer(
            BEACON_WND,
            TIMER_BEACON_DURATION,
            TIMER_BEACON_DURATION_STEP as u32,
            None,
        );
        // SetForegroundWindow(BEACON_WND); // 这个会发生焦点切换
        SetWindowPos(
            BEACON_WND,
            HWND_TOPMOST,
            BEACON_ARG_X as c_int,
            BEACON_ARG_Y as c_int,
            BEACON_WIDTH as c_int,
            BEACON_HEIGHT as c_int,
            SWP_SHOWWINDOW,
        );
        on_beacon_timer(BEACON_DURATION_LEFT_COUNT);
    }
}

fn create_beacon_wnd() {
    unsafe {
        if !BEACON_WND.is_null() {
            return;
        }
        let cls_name = to_wstring("rust_window_class");
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
            lpszClassName: cls_name.as_ptr(),
            hIconSm: std::ptr::null_mut(),
        };
        if RegisterClassExW(&wc) == 0 {
            #[cfg(debug_assertions)]
            {
                OutputDebugStringA(b"RegisterClassEx failed\0".as_ptr() as *const _);
            }
        }

        let wnd_name = to_wstring("Rust Window");
        // 透明要带WS_EX_LAYERED
        let hwnd = CreateWindowExW(
            WS_EX_TOOLWINDOW | WS_EX_TOPMOST | WS_EX_NOACTIVATE | WS_EX_LAYERED, // 任务栏无标题，显示时无焦点切换
            wc.lpszClassName,
            wnd_name.as_ptr(),
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
            200, // LWA_COLORKEY这个就没效果，是全透明的
            LWA_COLORKEY, //
                 // LWA_ALPHA,
        );
        BEACON_WND = hwnd;
    }
}

fn create_animation_wnd() {
    unsafe {
        if !ANIMATION_WND.is_null() {
            return;
        }
        let cls_name = to_wstring("rust_animation_window_class");
        let wc = WNDCLASSEXW {
            cbSize: std::mem::size_of::<WNDCLASSEXW>() as UINT,
            style: CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS,
            lpfnWndProc: Some(window_proc),
            cbClsExtra: 0,
            cbWndExtra: 0,
            hInstance: GetModuleHandleW(std::ptr::null_mut()) as HINSTANCE,
            hIcon: std::ptr::null_mut(),
            hCursor: LoadCursorW(std::ptr::null_mut(), IDC_ARROW),
            hbrBackground: GetStockObject(BLACK_BRUSH as i32) as HBRUSH,
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

        let wnd_name = to_wstring("Rust Window2");
        // 透明要带WS_EX_LAYERED
        let hwnd = CreateWindowExW(
            WS_EX_TOOLWINDOW | WS_EX_TOPMOST | WS_EX_NOACTIVATE | WS_EX_LAYERED, // 任务栏无标题，显示时无焦点切换
            wc.lpszClassName,
            wnd_name.as_ptr(),
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
            200, // LWA_COLORKEY这个就没效果，是全透明的
            LWA_COLORKEY, //
                 // LWA_ALPHA,
        );
        ANIMATION_WND = hwnd;

        // 直接覆盖整个屏幕
        let width = GetSystemMetrics(SM_CXSCREEN);
        let height = GetSystemMetrics(SM_CYSCREEN);
        SetWindowPos(
            ANIMATION_WND,
            HWND_TOPMOST,
            0,
            0,
            width,
            height,
            SWP_SHOWWINDOW,
        );
    }
}
fn show_animation_wnd() {
    unsafe {
        if ANIMATION_WND.is_null() {
            return;
        }
        #[cfg(debug_assertions)]
        {
            OutputDebugStringA(b"WM_SHOW_ANIMATION!\0" as *const _ as *const _);
        }
        ShowWindow(ANIMATION_WND, SW_SHOW);

        ANIMATION_DURATION_LEFT_COUNT =
            (ANIMATION_ARG_DURATION as f64 / ANIMATION_ARG_DURATION_STEP as f64) as isize;
        SetTimer(
            ANIMATION_WND,
            TIMER_ANIMATION_DURATION,
            TIMER_BEACON_DURATION_STEP as u32,
            None,
        );
        // TODO: 目前仅支持一次性设置
        if cursor_color.is_null() {
            cursor_color = CreateSolidBrush(RGB(cursor_color_r, cursor_color_g, cursor_color_b));
        }
        // SetForegroundWindow(ANIMATION_WND); // 这个会发生焦点切换
        on_animation_timer(ANIMATION_DURATION_LEFT_COUNT);
    }
}

// from https://docs.rs/qttypes/0.2.9/src/qttypes/lib.rs.html#923
#[repr(C)]
#[derive(Default, Clone, Copy, PartialEq, Debug)]
pub struct QPointF {
    pub x: f32,
    pub y: f32,
}
impl QPointF {
    fn new(x: f32, y: f32) -> QPointF {
        QPointF { x, y }
    }
}
impl std::ops::Add for QPointF {
    type Output = QPointF;
    fn add(self, other: QPointF) -> QPointF {
        QPointF {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}
impl std::ops::Sub for QPointF {
    type Output = QPointF;
    fn sub(self, other: QPointF) -> QPointF {
        QPointF {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}
impl std::ops::Mul<f32> for QPointF {
    type Output = QPointF;
    fn mul(self, other: f32) -> Self::Output {
        QPointF {
            x: self.x * other,
            y: self.y * other,
        }
    }
}

fn cursor_animation_draw_jelly_cursor(
    cs: QPointF,
    ce: QPointF,
    w: f32,
    h: f32,
    p: f32,
    points: &mut [QPointF; 4],
) {
    let diff = cs - ce;
    let diff_x = diff.x;
    let diff_y = diff.y;
    let w_point = QPointF::new(w, 0.0);
    let h_point = QPointF::new(0.0, h);
    let wh_point = QPointF::new(w, h);
    if diff_x * diff_y > 0.0 {
        points[0] = cs;
        points[1] = cs + wh_point;
        points[2] = ce + wh_point;
        points[3] = ce;
    } else if diff_x * diff_y < 0.0 {
        points[0] = cs + h_point;
        points[1] = cs + w_point;
        points[2] = ce + w_point;
        points[3] = ce + h_point;
    } else if diff_x == 0.0 {
        if diff_y >= 0.0 {
            points[0] = cs + h_point;
            points[1] = cs + wh_point;
            points[2] = ce + w_point;
            points[3] = ce;
        } else {
            points[0] = cs;
            points[1] = cs + w_point;
            points[2] = ce + wh_point;
            points[3] = ce + h_point;
        }
    } else if diff_y == 0.0 {
        if diff_x >= 0.0 {
            points[0] = cs + w_point;
            points[1] = cs + wh_point;
            points[2] = ce + h_point;
            points[3] = ce;
        } else {
            points[0] = cs;
            points[1] = cs + h_point;
            points[2] = ce + wh_point;
            points[3] = ce + w_point;
        }
    }

    if p < 0.5 {
        points[2] = points[2] * p * 2.0 + points[1] * (1.0 - p * 2.0);
        points[3] = points[3] * p * 2.0 + points[0] * (1.0 - p * 2.0);
    } else {
        points[0] = points[3] * (p - 0.5) * 2.0 + points[0] * (1.0 - (p - 0.5) * 2.0);
        points[1] = points[2] * (p - 0.5) * 2.0 + points[1] * (1.0 - (p - 0.5) * 2.0);
    }
}

fn on_animation_timer(left: isize) {
    unsafe {
        cursor_animation_percent = 1.0
            - (left as f32 / (ANIMATION_ARG_DURATION as f32 / ANIMATION_ARG_DURATION_STEP as f32));

        #[cfg(debug_assertions)]
        {
            let msg = format!("cursor_animation_percent:{}", cursor_animation_percent);
            OutputDebugStringW(to_wstring(&msg).as_slice().as_ptr());
        }

        // https://vc.zhizuobiao.com/vc-18112700241/
        let mut polygon: [QPointF; 4] = std::mem::zeroed();
        let p = cursor_animation_percent;
        let cs = QPointF::new(cursor_start_x as f32, cursor_start_y as f32);
        let ce = QPointF::new(cursor_end_x as f32, cursor_end_y as f32);
        cursor_animation_draw_jelly_cursor(
            cs,
            ce,
            cursor_info_w as f32,
            cursor_info_h as f32,
            p,
            &mut polygon,
        );

        #[cfg(debug_assertions)]
        {
            let msg = format!(
                "0:{:?}, 1:{:?}, 2:{:?}, 3: {:?}",
                polygon[0], polygon[1], polygon[2], polygon[3]
            );
            OutputDebugStringW(to_wstring(&msg).as_slice().as_ptr());
        }

        let hdc = GetDC(ANIMATION_WND);

        if !hdc.is_null() {
            // 强制刷新，不然会有拖影
            let bb = GetStockObject(BLACK_BRUSH as i32);
            let mut rc: RECT = std::mem::zeroed();
            GetClientRect(ANIMATION_WND, &mut rc);
            FillRect(hdc, &rc, bb as *mut _);

            // let bb = GetStockObject(WHITE_BRUSH as i32);
            let mut poly: [POINT; 4] = std::mem::zeroed();
            poly[0].x = polygon[0].x as i32;
            poly[0].y = polygon[0].y as i32;
            poly[1].x = polygon[1].x as i32;
            poly[1].y = polygon[1].y as i32;
            poly[2].x = polygon[2].x as i32;
            poly[2].y = polygon[2].y as i32;
            poly[3].x = polygon[3].x as i32;
            poly[3].y = polygon[3].y as i32;

            let rgn = CreatePolygonRgn(&poly as *const _, 4, WINDING);
            if !rgn.is_null() {
                FillRgn(hdc, rgn, cursor_color as *mut _);
                DeleteObject(rgn as *mut _);
            } else {
                // OutputDebugStringA(b"failed rng!\0".as_ptr() as *const _);
            }
            ReleaseDC(ANIMATION_WND, hdc);
        }
    }
}

pub fn becaon_init() {
    std::thread::spawn(move || unsafe {
        UI_THREAD_ID = GetCurrentThreadId();
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
                create_beacon_wnd();
                if !BEACON_WND.is_null() {
                    KillTimer(BEACON_WND, TIMER_BEACON_DURATION);
                    SetTimer(BEACON_WND, TIMER_BEACON_DELAY, msg.wParam as u32, None);
                } else {
                    #[cfg(debug_assertions)]
                    {
                        OutputDebugStringA(b"window is null!\0".as_ptr() as *const _);
                    }
                }
            } else if msg.message == WM_SHOW_ANIMATION {
                create_animation_wnd();
                if !ANIMATION_WND.is_null() {
                    KillTimer(ANIMATION_WND, TIMER_ANIMATION_DURATION);
                    show_animation_wnd();
                } else {
                    #[cfg(debug_assertions)]
                    {
                        OutputDebugStringA(b"animation window is null!\0".as_ptr() as *const _);
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
                if msg.wParam == TIMER_BEACON_DURATION {
                    BEACON_DURATION_LEFT_COUNT = BEACON_DURATION_LEFT_COUNT - 1;
                    if BEACON_DURATION_LEFT_COUNT < 0 {
                        #[cfg(debug_assertions)]
                        {
                            OutputDebugStringA(b"kill timer!\0" as *const _ as *const _);
                        }
                        ShowWindow(BEACON_WND, SW_HIDE);
                        KillTimer(BEACON_WND, TIMER_BEACON_DURATION);
                    } else {
                        on_beacon_timer(BEACON_DURATION_LEFT_COUNT);
                    }
                } else if msg.wParam == TIMER_BEACON_DELAY {
                    KillTimer(BEACON_WND, TIMER_BEACON_DELAY);
                    show_beacon_wnd();
                } else if msg.wParam == TIMER_ANIMATION_DURATION {
                    ANIMATION_DURATION_LEFT_COUNT = ANIMATION_DURATION_LEFT_COUNT - 1;
                    if ANIMATION_DURATION_LEFT_COUNT < 0 {
                        #[cfg(debug_assertions)]
                        {
                            OutputDebugStringA(b"kill timer!\0" as *const _ as *const _);
                        }
                        ShowWindow(ANIMATION_WND, SW_HIDE);
                        KillTimer(ANIMATION_WND, TIMER_ANIMATION_DURATION);
                    } else {
                        on_animation_timer(ANIMATION_DURATION_LEFT_COUNT);
                    }
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
            UI_THREAD_ID,
            WM_BEACON_SET_SIZE,
            width as WPARAM,
            height as LPARAM,
        );
        BEACON_R = r;
        BEACON_G = g;
        BEACON_B = b;
        TIMER_BEACON_DURATION_STEP = duration_step;
    }
    Ok(())
}

#[defun]
fn blink(x: usize, y: usize, timer: usize, delay: usize) -> Result<()> {
    unsafe {
        BEACON_ARG_X = x;
        BEACON_ARG_Y = y;
        BEACON_ARG_DURATION = timer;
        PostThreadMessageW(UI_THREAD_ID, WM_SHOW_BEACON, delay as WPARAM, 0 as LPARAM);
    }
    Ok(())
}

#[defun]
fn animation(
    x: usize,
    y: usize,
    w: usize,
    h: usize,
    timer: usize,
    step: usize,
    r: u8,
    g: u8,
    b: u8,
    diff_min: usize,
) -> Result<()> {
    unsafe {
        cursor_info_x = x;
        cursor_info_y = y;
        cursor_info_w = w;
        cursor_info_h = h;
        // 之前位置初始化，不用创建动画
        if cursor_info_prev_x == 0 && cursor_info_prev_y == 0 {
            cursor_info_prev_x = x;
            cursor_info_prev_y = y;
            return Ok(());
        }

        if cursor_info_x != cursor_info_prev_x || cursor_info_y != cursor_info_prev_y {
            if cursor_info_x.abs_diff(cursor_info_prev_x) >= diff_min
                || cursor_info_y.abs_diff(cursor_info_prev_y) >= diff_min
            {
                cursor_start_x = cursor_info_prev_x;
                cursor_start_y = cursor_info_prev_y;
                cursor_end_x = cursor_info_x;
                cursor_end_y = cursor_info_y;
                cursor_color_r = r;
                cursor_color_g = g;
                cursor_color_b = b;
                ANIMATION_ARG_DURATION = timer;
                ANIMATION_ARG_DURATION_STEP = step;
                PostThreadMessageW(UI_THREAD_ID, WM_SHOW_ANIMATION, 0 as WPARAM, 0 as LPARAM);
            }
            cursor_info_prev_x = x;
            cursor_info_prev_y = y;
            cursor_info_prev_w = w;
            cursor_info_prev_h = h;
        }
    }
    Ok(())
}
