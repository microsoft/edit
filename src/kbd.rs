//! The VK_* codes defined here match the VK_* constants on Windows.
//! It's a convenient way to handle keyboard input, even on other platforms.

use crate::input::{InputKey, InputKeyMod};

pub const VK_NULL: InputKey = InputKey::new('\0' as u32);
pub const VK_BACK: InputKey = InputKey::new(0x08);
pub const VK_TAB: InputKey = InputKey::new('\t' as u32);
pub const VK_RETURN: InputKey = InputKey::new('\r' as u32);
pub const VK_ESCAPE: InputKey = InputKey::new(0x1B);
pub const VK_SPACE: InputKey = InputKey::new(' ' as u32);
pub const VK_PRIOR: InputKey = InputKey::new(0x21);
pub const VK_NEXT: InputKey = InputKey::new(0x22);

pub const VK_END: InputKey = InputKey::new(0x23);
pub const VK_HOME: InputKey = InputKey::new(0x24);

pub const VK_LEFT: InputKey = InputKey::new(0x25);
pub const VK_UP: InputKey = InputKey::new(0x26);
pub const VK_RIGHT: InputKey = InputKey::new(0x27);
pub const VK_DOWN: InputKey = InputKey::new(0x28);

pub const VK_INSERT: InputKey = InputKey::new(0x2D);
pub const VK_DELETE: InputKey = InputKey::new(0x2E);

pub const VK_0: InputKey = InputKey::new('0' as u32);
pub const VK_1: InputKey = InputKey::new('1' as u32);
pub const VK_2: InputKey = InputKey::new('2' as u32);
pub const VK_3: InputKey = InputKey::new('3' as u32);
pub const VK_4: InputKey = InputKey::new('4' as u32);
pub const VK_5: InputKey = InputKey::new('5' as u32);
pub const VK_6: InputKey = InputKey::new('6' as u32);
pub const VK_7: InputKey = InputKey::new('7' as u32);
pub const VK_8: InputKey = InputKey::new('8' as u32);
pub const VK_9: InputKey = InputKey::new('9' as u32);

pub const VK_A: InputKey = InputKey::new('A' as u32);
pub const VK_B: InputKey = InputKey::new('B' as u32);
pub const VK_C: InputKey = InputKey::new('C' as u32);
pub const VK_D: InputKey = InputKey::new('D' as u32);
pub const VK_E: InputKey = InputKey::new('E' as u32);
pub const VK_F: InputKey = InputKey::new('F' as u32);
pub const VK_G: InputKey = InputKey::new('G' as u32);
pub const VK_H: InputKey = InputKey::new('H' as u32);
pub const VK_I: InputKey = InputKey::new('I' as u32);
pub const VK_J: InputKey = InputKey::new('J' as u32);
pub const VK_K: InputKey = InputKey::new('K' as u32);
pub const VK_L: InputKey = InputKey::new('L' as u32);
pub const VK_M: InputKey = InputKey::new('M' as u32);
pub const VK_N: InputKey = InputKey::new('N' as u32);
pub const VK_O: InputKey = InputKey::new('O' as u32);
pub const VK_P: InputKey = InputKey::new('P' as u32);
pub const VK_Q: InputKey = InputKey::new('Q' as u32);
pub const VK_R: InputKey = InputKey::new('R' as u32);
pub const VK_S: InputKey = InputKey::new('S' as u32);
pub const VK_T: InputKey = InputKey::new('T' as u32);
pub const VK_U: InputKey = InputKey::new('U' as u32);
pub const VK_V: InputKey = InputKey::new('V' as u32);
pub const VK_W: InputKey = InputKey::new('W' as u32);
pub const VK_X: InputKey = InputKey::new('X' as u32);
pub const VK_Y: InputKey = InputKey::new('Y' as u32);
pub const VK_Z: InputKey = InputKey::new('Z' as u32);

pub const VK_NUMPAD0: InputKey = InputKey::new(0x60);
pub const VK_NUMPAD1: InputKey = InputKey::new(0x61);
pub const VK_NUMPAD2: InputKey = InputKey::new(0x62);
pub const VK_NUMPAD3: InputKey = InputKey::new(0x63);
pub const VK_NUMPAD4: InputKey = InputKey::new(0x64);
pub const VK_NUMPAD5: InputKey = InputKey::new(0x65);
pub const VK_NUMPAD6: InputKey = InputKey::new(0x66);
pub const VK_NUMPAD7: InputKey = InputKey::new(0x67);
pub const VK_NUMPAD8: InputKey = InputKey::new(0x68);
pub const VK_NUMPAD9: InputKey = InputKey::new(0x69);
pub const VK_MULTIPLY: InputKey = InputKey::new(0x6A);
pub const VK_ADD: InputKey = InputKey::new(0x6B);
pub const VK_SEPARATOR: InputKey = InputKey::new(0x6C);
pub const VK_SUBTRACT: InputKey = InputKey::new(0x6D);
pub const VK_DECIMAL: InputKey = InputKey::new(0x6E);
pub const VK_DIVIDE: InputKey = InputKey::new(0x6F);

pub const VK_F1: InputKey = InputKey::new(0x70);
pub const VK_F2: InputKey = InputKey::new(0x71);
pub const VK_F3: InputKey = InputKey::new(0x72);
pub const VK_F4: InputKey = InputKey::new(0x73);
pub const VK_F5: InputKey = InputKey::new(0x74);
pub const VK_F6: InputKey = InputKey::new(0x75);
pub const VK_F7: InputKey = InputKey::new(0x76);
pub const VK_F8: InputKey = InputKey::new(0x77);
pub const VK_F9: InputKey = InputKey::new(0x78);
pub const VK_F10: InputKey = InputKey::new(0x79);
pub const VK_F11: InputKey = InputKey::new(0x7A);
pub const VK_F12: InputKey = InputKey::new(0x7B);
pub const VK_F13: InputKey = InputKey::new(0x7C);
pub const VK_F14: InputKey = InputKey::new(0x7D);
pub const VK_F15: InputKey = InputKey::new(0x7E);
pub const VK_F16: InputKey = InputKey::new(0x7F);
pub const VK_F17: InputKey = InputKey::new(0x80);
pub const VK_F18: InputKey = InputKey::new(0x81);
pub const VK_F19: InputKey = InputKey::new(0x82);
pub const VK_F20: InputKey = InputKey::new(0x83);
pub const VK_F21: InputKey = InputKey::new(0x84);
pub const VK_F22: InputKey = InputKey::new(0x85);
pub const VK_F23: InputKey = InputKey::new(0x86);
pub const VK_F24: InputKey = InputKey::new(0x87);

pub const MOD_NONE: InputKeyMod = InputKeyMod::new(0x00000000);
pub const MOD_CTRL: InputKeyMod = InputKeyMod::new(0x01000000);
pub const MOD_ALT: InputKeyMod = InputKeyMod::new(0x02000000);
pub const MOD_SHIFT: InputKeyMod = InputKeyMod::new(0x04000000);

pub const MOD_CTRL_ALT: InputKeyMod = InputKeyMod::new(0x03000000);
pub const MOD_CTRL_SHIFT: InputKeyMod = InputKeyMod::new(0x05000000);
pub const MOD_ALT_SHIFT: InputKeyMod = InputKeyMod::new(0x06000000);
pub const MOD_CTRL_ALT_SHIFT: InputKeyMod = InputKeyMod::new(0x07000000);
