use std::ffi::CStr;
use std::os::raw::c_char;
use std::sync::Mutex;

use crossterm::{
    cursor::MoveTo,
    event::{Event, KeyCode, read},
    execute, queue,
    style::Print,
    terminal::{Clear, ClearType, disable_raw_mode, enable_raw_mode},
};

#[unsafe(no_mangle)]
pub extern "C" fn gimlight_set_raw_mode_raw() -> u32 {
    set_raw_mode_impl()
}

#[unsafe(no_mangle)]
pub extern "C" fn gimlight_restore_terminal_raw() -> u32 {
    restore_terminal_impl()
}

#[unsafe(no_mangle)]
/// # Safety
///
/// `text` must point to a valid, readable, NUL-terminated C string for the
/// duration of this call.
pub unsafe extern "C" fn gimlight_render_text_raw(text: *const c_char) -> u32 {
    if text.is_null() {
        return ERROR_TERMINAL_IO;
    }

    let text = match unsafe { CStr::from_ptr(text) }.to_str() {
        Ok(text) => text,
        Err(_) => return ERROR_TERMINAL_IO,
    };

    render_text_impl(text)
}

#[unsafe(no_mangle)]
pub extern "C" fn gimlight_read_input_raw() -> u32 {
    read_input_impl()
}

const OK: u32 = 0;

const ERROR_MUTEX_POISONED: u32 = 1_000;
const ERROR_TERMINAL_IO: u32 = 1_001;

const INPUT_LEFT: u32 = 1;
const INPUT_RIGHT: u32 = 2;
const INPUT_UP: u32 = 3;
const INPUT_DOWN: u32 = 4;
const INPUT_QUIT: u32 = 5;
const INPUT_UNKNOWN: u32 = 6;
const INPUT_ERROR: u32 = 7;

static RAW_MODE_ENABLED: Mutex<bool> = Mutex::new(false);

fn set_raw_mode_impl() -> u32 {
    let mut enabled = match RAW_MODE_ENABLED.lock() {
        Ok(enabled) => enabled,
        Err(_) => return ERROR_MUTEX_POISONED,
    };

    if *enabled {
        return OK;
    }

    match enable_raw_mode() {
        Ok(()) => {
            *enabled = true;
            OK
        }
        Err(error) => terminal_io_error_code(error),
    }
}

fn restore_terminal_impl() -> u32 {
    let mut enabled = match RAW_MODE_ENABLED.lock() {
        Ok(enabled) => enabled,
        Err(_) => return ERROR_MUTEX_POISONED,
    };

    if !*enabled {
        return OK;
    }

    match disable_raw_mode() {
        Ok(()) => {
            *enabled = false;
            OK
        }
        Err(error) => terminal_io_error_code(error),
    }
}

fn terminal_io_error_code(error: std::io::Error) -> u32 {
    error
        .raw_os_error()
        .map(|code| code as u32)
        .unwrap_or(ERROR_TERMINAL_IO)
}

fn render_text_impl(text: &str) -> u32 {
    let mut stdout = std::io::stdout();

    if clear_screen(&mut stdout).is_err() || print_text(&mut stdout, text).is_err() {
        return ERROR_TERMINAL_IO;
    }

    match std::io::Write::flush(&mut stdout) {
        Ok(()) => OK,
        Err(_) => ERROR_TERMINAL_IO,
    }
}

fn clear_screen(stdout: &mut std::io::Stdout) -> std::io::Result<()> {
    execute!(stdout, Clear(ClearType::All), MoveTo(0, 0))
}

fn print_text(stdout: &mut std::io::Stdout, text: &str) -> std::io::Result<()> {
    queue!(stdout, Print(text))
}

fn read_input_impl() -> u32 {
    match read() {
        Ok(Event::Key(event)) => input_code_from_key(event.code),
        Ok(_) => INPUT_UNKNOWN,
        Err(_) => INPUT_ERROR,
    }
}

fn input_code_from_key(code: KeyCode) -> u32 {
    match code {
        KeyCode::Left | KeyCode::Char('h') => INPUT_LEFT,
        KeyCode::Right | KeyCode::Char('l') => INPUT_RIGHT,
        KeyCode::Up | KeyCode::Char('k') => INPUT_UP,
        KeyCode::Down | KeyCode::Char('j') => INPUT_DOWN,
        KeyCode::Char('q') => INPUT_QUIT,
        _ => INPUT_UNKNOWN,
    }
}
