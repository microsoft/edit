use crate::apperr;
use crate::helpers::Size;
use crate::input::{Input, InputKey, InputKeyMod, InputText, kbmod, vk};
use r_efi::efi;
use std::ffi::{CStr, c_void};
use std::fmt::Write;
use std::fs::File;
use std::mem::MaybeUninit;
use std::os::uefi as uefi_std;
use std::path::{Path, PathBuf};
use std::ptr::NonNull;
use std::time;
use uefi::boot::{OpenProtocolAttributes, ScopedProtocol};
use uefi::proto::console::text::{Color, Key};
use uefi::{Handle, ResultExt};

pub fn preferred_languages() -> Vec<String> {
    vec!["en".to_string()]
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[repr(C)]
pub struct KeyState {
    pub shift_state: u32,
    pub toggle_state: u8,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[repr(C)]
pub struct KeyData {
    pub key: uefi_raw::protocol::console::InputKey,
    pub state: KeyState,
}

#[derive(Debug)]
#[repr(C)]
pub struct SimpleTextInputProtocolEx {
    pub reset_ex:
        unsafe extern "efiapi" fn(this: *mut Self, extended_verification: bool) -> uefi_raw::Status,
    pub read_key_stroke_ex:
        unsafe extern "efiapi" fn(this: *mut Self, key: *mut KeyData) -> uefi_raw::Status,
    pub wait_for_key_ex: uefi_raw::Event,
}

impl SimpleTextInputProtocolEx {
    pub const GUID: uefi_raw::Guid = uefi_raw::guid!("dd9e7534-7762-4698-8c14-f58517a625aa");
}

#[derive(Debug)]
#[repr(transparent)]
#[uefi::proto::unsafe_protocol(SimpleTextInputProtocolEx::GUID)]
pub struct SimpleTextInputEx(SimpleTextInputProtocolEx);

pub fn init() -> apperr::Result<()> {
    let st = uefi_std::env::system_table();
    let ih = uefi_std::env::image_handle();

    // Mandatory setup code for `uefi` crate.
    unsafe {
        uefi::table::set_system_table(st.as_ptr().cast());

        let ih = Handle::from_ptr(ih.as_ptr().cast()).unwrap();
        uefi::boot::set_image_handle(ih);
    }

    let _ = uefi::helpers::init();
    Ok({})
}

pub fn deinit() {
    // Nothing to do
}

pub fn inject_window_size_into_stdin() {
    unsafe {
        STATE.inject_resize = true;
    }
}

fn get_window_size() -> (u16, u16) {
    // We return the number of rows minus one because the UEFI console acts like
    // the Windows console, where there is no deferred EOL wrap position.
    // Returning the full number of rows makes us push the top line off the screen
    // when we draw the final line.
    match uefi::system::with_stdout(|o| o.current_mode().unwrap()) {
        Some(t) => (t.columns() as u16, t.rows() as u16 - 1),
        None => (80, 24),
    }
}

struct State {
    inject_resize: bool,
    last_text: [u8; 4],
    last_text_len: usize,
}

static mut STATE: State = State {
    inject_resize: false,
    last_text: [0; 4],
    last_text_len: 0,
};

pub fn read_stdin(_timeout: time::Duration) -> Option<String> {
    // This platform uses raw input
    None
}

fn wait_and_read_single_key_ex() -> Option<KeyData> {
    let mut p = unsafe {
        uefi::boot::open_protocol::<SimpleTextInputEx>(
            uefi::boot::OpenProtocolParams {
                handle: uefi::Handle::from_ptr(
                    uefi::table::system_table_raw()
                        .unwrap()
                        .as_ref()
                        .stdin_handle,
                )
                .unwrap(),
                agent: uefi::boot::image_handle(),
                controller: None,
            },
            OpenProtocolAttributes::GetProtocol,
        )
    }
    .ok()?;
    let mut events = [unsafe { uefi::Event::from_ptr(p.0.wait_for_key_ex) }?];
    uefi::boot::wait_for_event(&mut events).ok()?;
    let mut key = MaybeUninit::<KeyData>::uninit();
    match unsafe { (p.0.read_key_stroke_ex)(&mut p.0, key.as_mut_ptr()) } {
        uefi_raw::Status::NOT_READY => None,
        _ => Some(unsafe { key.assume_init() }),
    }
}

#[allow(static_mut_refs)]
fn transform_single_key(key: &KeyData) -> Option<Input<'static>> {
    const LUT: [u8; 0x18] = [
        0,
        vk::UP.value() as u8,     // 1
        vk::DOWN.value() as u8,   // 2
        vk::RIGHT.value() as u8,  // 3
        vk::LEFT.value() as u8,   // 4
        vk::HOME.value() as u8,   // 5
        vk::END.value() as u8,    // 6
        vk::INSERT.value() as u8, // 7
        vk::DELETE.value() as u8, // 8
        vk::PRIOR.value() as u8,  // 9
        vk::NEXT.value() as u8,   // 10
        vk::F1.value() as u8,     // 11
        vk::F2.value() as u8,     // 12
        vk::F3.value() as u8,     // 13
        vk::F4.value() as u8,     // 14
        vk::F5.value() as u8,     // 15
        vk::F6.value() as u8,     // 16
        vk::F7.value() as u8,     // 17
        vk::F8.value() as u8,     // 18
        vk::F9.value() as u8,     // 19
        vk::F10.value() as u8,    // 20
        vk::F11.value() as u8,    // 21
        vk::F12.value() as u8,    // 22
        vk::ESCAPE.value() as u8, // 23
    ];
    const LUT_LEN: i32 = LUT.len() as i32;

    let mut m = kbmod::NONE;
    if (key.state.shift_state & 0x80000000) != 0 {
        m |= if (key.state.shift_state & 0x3) != 0 {
            kbmod::SHIFT
        } else {
            kbmod::NONE
        };
        m |= if (key.state.shift_state & 0xc) != 0 {
            kbmod::CTRL
        } else {
            kbmod::NONE
        };
        m |= if (key.state.shift_state & 0x30) != 0 {
            kbmod::ALT
        } else {
            kbmod::NONE
        };
    }

    unsafe {
        match key.key.scan_code as i32 {
            0 => {
                if m == kbmod::NONE && key.key.unicode_char >= 0x20 {
                    let v = char::from_u32(key.key.unicode_char as u32)?;
                    let s: String = v.to_string();
                    STATE.last_text_len = s.len();
                    STATE.last_text[..STATE.last_text_len]
                        .copy_from_slice(&s.as_bytes()[..STATE.last_text_len]);
                    Some(Input::Text(InputText {
                        text: str::from_utf8_unchecked(&STATE.last_text[..STATE.last_text_len]),
                        bracketed: false,
                    }))
                } else {
                    Some(Input::Keyboard(
                        InputKey::from_ascii(char::from_u32(key.key.unicode_char as u32)?)
                            .unwrap_or_else(|| InputKey::new(key.key.unicode_char as u32))
                            .with_modifiers(m),
                    ))
                }
            }
            1..LUT_LEN => Some(Input::Keyboard(
                InputKey::new(LUT[key.key.scan_code as usize] as u32).with_modifiers(m),
            )),
            _ => {
                println!("UNKNOWN SCAN {:x}\r\n", key.key.scan_code);
                None
            }
        }
    }
}

#[allow(static_mut_refs)]
pub fn read_input() -> Option<Input<'static>> {
    unsafe {
        if STATE.inject_resize {
            STATE.inject_resize = false;
            let t = get_window_size();
            Some(Input::Resize(Size {
                width: t.0 as i32,
                height: t.1 as i32,
            }))
        } else {
            transform_single_key(&wait_and_read_single_key_ex()?)
        }
    }
}

static RGB_FG_INDEX: &'static [u8] = &[
    0, 1, 1, 9, 0, 0, 1, 1, 2, 1, 1, 1, 2, 8, 1, 9, 2, 2, 3, 3, 2, 2, 11, 3, 10, 10, 11, 11, 10,
    10, 10, 11, 0, 5, 1, 1, 0, 0, 1, 1, 8, 1, 1, 1, 2, 8, 1, 9, 2, 2, 3, 3, 2, 2, 11, 3, 10, 10,
    10, 11, 10, 10, 10, 11, 5, 5, 5, 1, 4, 5, 1, 1, 8, 8, 1, 9, 2, 8, 9, 9, 2, 2, 3, 3, 2, 2, 11,
    3, 10, 10, 11, 11, 10, 10, 10, 11, 4, 5, 5, 1, 4, 5, 5, 1, 8, 5, 5, 1, 8, 8, 9, 9, 2, 2, 8, 9,
    10, 2, 11, 3, 10, 10, 11, 11, 10, 10, 10, 11, 4, 13, 5, 5, 4, 13, 5, 5, 4, 13, 13, 13, 6, 8,
    13, 9, 6, 8, 8, 9, 10, 10, 11, 3, 10, 10, 11, 11, 10, 10, 10, 11, 4, 13, 13, 13, 4, 13, 13, 13,
    4, 12, 13, 13, 6, 12, 13, 13, 6, 6, 8, 9, 6, 6, 7, 7, 10, 14, 14, 7, 10, 10, 14, 11, 4, 12, 13,
    13, 4, 12, 13, 13, 4, 12, 13, 13, 6, 12, 12, 13, 6, 6, 12, 7, 6, 6, 7, 7, 6, 14, 14, 7, 14, 14,
    14, 15, 12, 12, 13, 13, 12, 12, 13, 13, 12, 12, 12, 13, 12, 12, 12, 13, 6, 12, 12, 7, 6, 6, 7,
    7, 6, 14, 14, 7, 14, 14, 14, 15,
];

pub fn move_cursor(x: usize, y: usize) {
    let _ = uefi::system::with_stdout(|o| o.set_cursor_position(x, y));
}

pub fn color_to_index(color: u32) -> u8 {
    // ooBBGGRR
    let r = (color & 0xFF) as u8;
    let g = ((color & 0xFF00) >> 8) as u8;
    let b = ((color & 0xFF0000) >> 16) as u8;
    let crgb = (r & 0b11100000) | ((g >> 3) & 0b11100) | ((b >> 6) & 0b11);
    RGB_FG_INDEX[crgb as usize]
}

pub fn index_to_uefi(index: u8) -> Color {
    unsafe { std::mem::transmute::<_, Color>(index) }
}

pub fn set_color(fg: u32, bg: u32) {
    let f = color_to_index(fg);
    let b = color_to_index(bg) & 0b111; // cut off the intensity flag
    let _ = uefi::system::with_stdout(|o| o.set_color(index_to_uefi(f), index_to_uefi(b)));
}

pub fn reset() {
    let _ = uefi::system::with_stdout(|o| o.clear());
}

pub fn set_cursor(en: bool) {
    let _ = uefi::system::with_stdout(|o| o.enable_cursor(en));
}

pub fn write_stdout(text: &str) {
    match uefi::system::with_stdout(|o| o.write_str(text)) {
        Ok(t) => _ = t,
        _ => unreachable!(),
    }
}

pub fn open_stdin_if_redirected() -> Option<File> {
    // This platform does not support redirection
    None
}

pub unsafe fn virtual_reserve(size: usize) -> apperr::Result<NonNull<u8>> {
    Ok(uefi::boot::allocate_pool(uefi::boot::MemoryType::BOOT_SERVICES_DATA, size).unwrap())
}

pub unsafe fn virtual_release(base: NonNull<u8>, _size: usize) {
    let _ = unsafe { uefi::boot::free_pool(base) }.unwrap();
}

pub unsafe fn virtual_commit(_base: NonNull<u8>, _size: usize) -> apperr::Result<()> {
    Ok({})
}

// It'd be nice to constrain T to std::marker::FnPtr, but that's unstable.
pub fn get_proc_address<T>(_handle: NonNull<c_void>, _name: &CStr) -> apperr::Result<T> {
    Err(apperr::Error::new_sys(
        efi::Status::NOT_FOUND.as_usize() as u32
    ))
}

pub fn load_libicuuc() -> apperr::Result<NonNull<c_void>> {
    Err(apperr::Error::new_sys(
        efi::Status::NOT_FOUND.as_usize() as u32
    ))
}

pub fn load_libicui18n() -> apperr::Result<NonNull<c_void>> {
    Err(apperr::Error::new_sys(
        efi::Status::NOT_FOUND.as_usize() as u32
    ))
}

pub fn apperr_format(code: u32) -> String {
    let errno = code & 0xFFFF;
    let result = format!("Error {:x}", errno);

    result
}

pub fn io_error_to_apperr(err: std::io::Error) -> apperr::Error {
    apperr::Error::new_sys(
        (err.raw_os_error().unwrap_or((err.kind() as usize) | 0xFF00) as u32).max(1),
    )
}

pub fn switch_modes() -> apperr::Result<()> {
    Ok({})
}

pub fn canonicalize<P: AsRef<Path>>(path: P) -> apperr::Result<PathBuf> {
    Ok(PathBuf::from(path.as_ref()))
}

pub fn apperr_is_not_found(err: apperr::Error) -> bool {
    match err {
        apperr::Error::Sys(v) => v == efi::Status::NOT_FOUND.as_usize() as u32,
        _ => false,
    }
}

pub enum DrawingOp<'a> {
    Text(&'a str),
    Color(u32, u32),
    MoveTo(usize, usize),
    Cursor(bool),
}

pub fn render(ops: &[DrawingOp]) {
    // TODO - these two are hardcoded for UEFI but we
    // need to know the palette to figure that out
    let mut last_bg: u32 = 0xff000000;
    let mut last_fg: u32 = 0xffbebebe;
    for op in ops {
        match op {
            DrawingOp::Text(s) => {
                write_stdout(s);
            }
            DrawingOp::Color(f, b) => {
                let mut changed = false;
                if *f != last_fg {
                    last_fg = *f;
                    changed = true;
                }
                if *b != last_bg {
                    last_bg = *b;
                    changed = true;
                }
                if changed {
                    set_color(last_fg, last_bg);
                }
            }
            DrawingOp::MoveTo(x, y) => move_cursor(*x, *y),
            DrawingOp::Cursor(v) => set_cursor(*v),
        }
    }
}
