// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! LSH bytecode interpreter.
//!
//! ## Performance notes
//!
//! - The main loop is "unsafe". Profile before "cleaning" it up.
//! - `charset_gobble`, `inlined_mem(i)cmp` are hot paths.
//!
//! ## Instruction encoding
//!
//! Variable-length encoding, 1-9 bytes per instruction. See [`Instruction::encode`].
//!
//! ## Gotchas
//!
//! - `Return` with empty stack resets the VM to entrypoint and clears registers.
//!   This is how the DSL returns to the "idle" state between tokens.
//! - `AwaitInput` only breaks the loop if `off >= line.len()`. If not at EOL, it's a no-op.
//!   This allows the DSL to say "wait for more input OR continue if there is some".
//! - The result always has a sentinel span at `line.len()`. Consumers can rely on this.
//! - [`Instruction::address_offset`] returns where, within an instruction, the jump target lives,
//!   as used by the backend's relocation system.

use std::fmt;

use stdext::arena::Arena;
use stdext::arena_write_fmt;
use stdext::collections::{BString, BVec};

/// A compiled language definition with its bytecode entrypoint.
pub struct Language {
    /// Unique identifier (e.g., "rust", "markdown").
    pub id: &'static str,
    /// Human-readable display name.
    pub name: &'static str,
    /// Bytecode address where execution begins for this language.
    pub entrypoint: u32,
}

impl PartialEq for &'static Language {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

/// A highlight span indicating that text from `start` to the next span has the given `kind`.
///
/// Spans are half-open: `[start, next.start)`. The final span in a line extends to EOL.
#[derive(Clone, PartialEq, Eq)]
pub struct Highlight<T> {
    /// Byte offset where this highlight begins.
    pub start: usize,
    /// The token/highlight type (e.g., keyword, string, comment).
    pub kind: T,
}

impl<T: fmt::Debug> fmt::Debug for Highlight<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {:?})", self.start, self.kind)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    // These two registers are shared across function calls...
    InputOffset,
    HighlightStart,
    // ...and the rest is caller-saved.
    ProgramCounter,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
}

impl Register {
    pub const FIRST_USER_REG: usize = 3; // aka x3
    pub const COUNT: usize = 16;

    #[inline(always)]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(value < Self::COUNT);
        unsafe { std::mem::transmute::<u8, Register>(value as u8) }
    }

    pub fn mnemonic(&self) -> &'static str {
        match self {
            Register::InputOffset => "off",
            Register::HighlightStart => "hs",
            Register::ProgramCounter => "pc",
            Register::X3 => "x3",
            Register::X4 => "x4",
            Register::X5 => "x5",
            Register::X6 => "x6",
            Register::X7 => "x7",
            Register::X8 => "x8",
            Register::X9 => "x9",
            Register::X10 => "x10",
            Register::X11 => "x11",
            Register::X12 => "x12",
            Register::X13 => "x13",
            Register::X14 => "x14",
            Register::X15 => "x15",
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.mnemonic())
    }
}

#[repr(C)]
#[derive(Default, Clone, Copy)]
pub struct Registers {
    pub off: u32, // x0 = InputOffset
    pub hs: u32,  // x1 = HighlightStart
    pub pc: u32,  // x2 = ProgramCounter
    pub x3: u32,
    pub x4: u32,
    pub x5: u32,
    pub x6: u32,
    pub x7: u32,
    pub x8: u32,
    pub x9: u32,
    pub x10: u32,
    pub x11: u32,
    pub x12: u32,
    pub x13: u32,
    pub x14: u32,
    pub x15: u32,
}

impl Registers {
    #[inline(always)]
    pub fn get(&self, reg: Register) -> u32 {
        unsafe { self.as_ptr().add(reg as usize).read() }
    }

    #[inline(always)]
    pub fn set(&mut self, reg: Register, val: u32) {
        unsafe { self.as_mut_ptr().add(reg as usize).write(val) }
    }

    #[inline(always)]
    unsafe fn as_ptr(&self) -> *const u32 {
        self as *const _ as *const u32
    }

    #[inline(always)]
    unsafe fn as_mut_ptr(&mut self) -> *mut u32 {
        self as *mut _ as *mut u32
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // NOTE: This allows for jumps by manipulating Register::ProgramCounter.
    Mov { dst: Register, src: Register },
    Add { dst: Register, src: Register },
    Sub { dst: Register, src: Register },
    MovImm { dst: Register, imm: u32 },
    AddImm { dst: Register, imm: u32 },
    SubImm { dst: Register, imm: u32 },

    Call { tgt: u32 },
    Return,

    JumpEQ { lhs: Register, rhs: Register, tgt: u32 }, // ==
    JumpNE { lhs: Register, rhs: Register, tgt: u32 }, // !=
    JumpLT { lhs: Register, rhs: Register, tgt: u32 }, // <
    JumpLE { lhs: Register, rhs: Register, tgt: u32 }, // <=
    JumpGT { lhs: Register, rhs: Register, tgt: u32 }, // >
    JumpGE { lhs: Register, rhs: Register, tgt: u32 }, // >=

    // Jumps to `tgt` if we're at the end of the line.
    JumpIfEndOfLine { tgt: u32 },

    // Jumps to `tgt` if the test succeeds.
    // `idx` specifies the charset/string to use.
    JumpIfMatchCharset { idx: u32, min: u32, max: u32, tgt: u32 },
    JumpIfMatchPrefix { idx: u32, tgt: u32 },
    JumpIfMatchPrefixInsensitive { idx: u32, tgt: u32 },

    // Flushes the current HighlightKind to the output.
    FlushHighlight { kind: Register },

    // Awaits more input to be available.
    AwaitInput,
}

macro_rules! instruction_decode {
    ($assembly:expr, $pc:expr, {
        Mov { $mov_dst:ident, $mov_src:ident } => $mov_handler:block
        Add { $add_dst:ident, $add_src:ident } => $add_handler:block
        Sub { $sub_dst:ident, $sub_src:ident } => $sub_handler:block
        MovImm { $movi_dst:ident, $movi_imm:ident } => $movi_handler:block
        AddImm { $addi_dst:ident, $addi_imm:ident } => $addi_handler:block
        SubImm { $subi_dst:ident, $subi_imm:ident } => $subi_handler:block

        Call { $call_tgt:ident } => $call_handler:block
        Return => $ret_handler:block

        JumpEQ { $jeq_lhs:ident, $jeq_rhs:ident, $jeq_tgt:ident } => $jeq_handler:block
        JumpNE { $jne_lhs:ident, $jne_rhs:ident, $jne_tgt:ident } => $jne_handler:block
        JumpLT { $jlt_lhs:ident, $jlt_rhs:ident, $jlt_tgt:ident } => $jlt_handler:block
        JumpLE { $jle_lhs:ident, $jle_rhs:ident, $jle_tgt:ident } => $jle_handler:block
        JumpGT { $jgt_lhs:ident, $jgt_rhs:ident, $jgt_tgt:ident } => $jgt_handler:block
        JumpGE { $jge_lhs:ident, $jge_rhs:ident, $jge_tgt:ident } => $jge_handler:block

        JumpIfEndOfLine { $jeol_tgt:ident } => $jeol_handler:block

        JumpIfMatchCharset { $jc_idx:ident, $jc_min:ident, $jc_max:ident, $jc_tgt:ident } => $jc_handler:block
        JumpIfMatchPrefix { $jp_idx:ident, $jp_tgt:ident } => $jp_handler:block
        JumpIfMatchPrefixInsensitive { $jpi_idx:ident, $jpi_tgt:ident } => $jpi_handler:block

        FlushHighlight { $flush_kind:ident } => $flush_handler:block
        AwaitInput => $await_handler:block

        _ => $bad_opcode:expr $(,)?
    }) => {{
        #[inline(always)]
        fn dec_reg_single(bytes: &[u8], off: usize) -> Register {
            let b = unsafe { *bytes.as_ptr().add(off) as usize };
            Register::from_usize(b & 0xf)
        }

        #[inline(always)]
        fn dec_reg_pair(bytes: &[u8], off: usize) -> (Register, Register) {
            let b = unsafe { *bytes.as_ptr().add(off) as usize };
            let dst = Register::from_usize(b & 0xf);
            let src = Register::from_usize(b >> 4);
            (dst, src)
        }

        #[inline(always)]
        fn dec_u32(bytes: &[u8], off: usize) -> u32 {
            unsafe { (bytes.as_ptr().add(off) as *const u32).read_unaligned() }
        }

        let __asm: &[u8] = $assembly;
        let __off = $pc as usize;

        // The use of unsafe code above boosts performance by about 10%. We rely on the code
        // generator to emit invalid 0xff opcodes at the end of the instruction stream as padding.
        // This way we can read past-the-end, even if the last non-0xff instruction is truncated.
        let __opcode = __asm[__off];

        match __opcode {
            0 => {
                // Mov
                $pc += 2;
                let ($mov_dst, $mov_src) = dec_reg_pair(__asm, __off + 1);
                $mov_handler
            }
            1 => {
                // Add
                $pc += 2;
                let ($add_dst, $add_src) = dec_reg_pair(__asm, __off + 1);
                $add_handler
            }
            2 => {
                // Sub
                $pc += 2;
                let ($sub_dst, $sub_src) = dec_reg_pair(__asm, __off + 1);
                $sub_handler
            }
            3 => {
                // MovImm
                $pc += 6;
                let $movi_dst = dec_reg_single(__asm, __off + 1);
                let $movi_imm = dec_u32(__asm, __off + 2);
                $movi_handler
            }
            4 => {
                // AddImm
                $pc += 6;
                let $addi_dst = dec_reg_single(__asm, __off + 1);
                let $addi_imm = dec_u32(__asm, __off + 2);
                $addi_handler
            }
            5 => {
                // SubImm
                $pc += 6;
                let $subi_dst = dec_reg_single(__asm, __off + 1);
                let $subi_imm = dec_u32(__asm, __off + 2);
                $subi_handler
            }

            6 => {
                // Call
                $pc += 5;
                let $call_tgt = dec_u32(__asm, __off + 1);
                $call_handler
            }
            7 => {
                // Return
                $pc += 1;
                $ret_handler
            }

            8 => {
                // JumpEQ
                $pc += 6;
                let ($jeq_lhs, $jeq_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jeq_tgt = dec_u32(__asm, __off + 2);
                $jeq_handler
            }
            9 => {
                // JumpNE
                $pc += 6;
                let ($jne_lhs, $jne_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jne_tgt = dec_u32(__asm, __off + 2);
                $jne_handler
            }
            10 => {
                // JumpLT
                $pc += 6;
                let ($jlt_lhs, $jlt_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jlt_tgt = dec_u32(__asm, __off + 2);
                $jlt_handler
            }
            11 => {
                // JumpLE
                $pc += 6;
                let ($jle_lhs, $jle_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jle_tgt = dec_u32(__asm, __off + 2);
                $jle_handler
            }
            12 => {
                // JumpGT
                $pc += 6;
                let ($jgt_lhs, $jgt_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jgt_tgt = dec_u32(__asm, __off + 2);
                $jgt_handler
            }
            13 => {
                // JumpGE
                $pc += 6;
                let ($jge_lhs, $jge_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jge_tgt = dec_u32(__asm, __off + 2);
                $jge_handler
            }

            14 => {
                // JumpIfEndOfLine
                $pc += 5;
                let $jeol_tgt = dec_u32(__asm, __off + 1);
                $jeol_handler
            }

            15 => {
                // JumpIfMatchCharset
                $pc += 17;
                let $jc_idx = dec_u32(__asm, __off + 1);
                let $jc_min = dec_u32(__asm, __off + 5);
                let $jc_max = dec_u32(__asm, __off + 9);
                let $jc_tgt = dec_u32(__asm, __off + 13);
                $jc_handler
            }
            16 => {
                // JumpIfMatchPrefix
                $pc += 9;
                let $jp_idx = dec_u32(__asm, __off + 1);
                let $jp_tgt = dec_u32(__asm, __off + 5);
                $jp_handler
            }
            17 => {
                // JumpIfMatchPrefixInsensitive
                $pc += 9;
                let $jpi_idx = dec_u32(__asm, __off + 1);
                let $jpi_tgt = dec_u32(__asm, __off + 5);
                $jpi_handler
            }

            18 => {
                // FlushHighlight
                $pc += 2;
                let $flush_kind = dec_reg_single(__asm, __off + 1);
                $flush_handler
            }
            19 => {
                // AwaitInput
                $pc += 1;
                $await_handler
            }

            _ => $bad_opcode,
        }
    }};
}

impl Instruction {
    // JumpIfMatchCharset, etc., are 1 byte opcode + 4 u32 parameters.
    pub const MAX_ENCODED_SIZE: usize = 1 + 4 * 4;

    pub fn address_offset(&self) -> Option<usize> {
        match *self {
            Instruction::MovImm { .. }
            | Instruction::AddImm { .. }
            | Instruction::SubImm { .. } => Some(1 + 1), // opcode + dst

            Instruction::Call { .. } => Some(1), // opcode

            Instruction::JumpEQ { .. }
            | Instruction::JumpNE { .. }
            | Instruction::JumpLT { .. }
            | Instruction::JumpLE { .. }
            | Instruction::JumpGT { .. }
            | Instruction::JumpGE { .. } => Some(1 + 1), // opcode + lhs/rhs pair

            Instruction::JumpIfEndOfLine { .. } => Some(1), // opcode

            Instruction::JumpIfMatchCharset { .. } => Some(1 + 3 * 4), // opcode + idx + min + max
            Instruction::JumpIfMatchPrefix { .. }
            | Instruction::JumpIfMatchPrefixInsensitive { .. } => Some(1 + 4), // opcode + idx

            _ => None,
        }
    }

    #[allow(clippy::identity_op)]
    pub fn encode<'a>(&self, arena: &'a Arena) -> BVec<'a, u8> {
        fn enc_reg_pair(lo: Register, hi: Register) -> u8 {
            ((hi as u8) << 4) | (lo as u8)
        }

        fn enc_reg_single(lo: Register) -> u8 {
            lo as u8
        }

        fn enc_u32(val: u32) -> [u8; 4] {
            val.to_le_bytes()
        }

        let mut bytes = BVec::empty();
        #[allow(clippy::missing_transmute_annotations)]
        bytes.push(arena, unsafe { std::mem::transmute(std::mem::discriminant(self)) });

        match *self {
            Instruction::Mov { dst, src }
            | Instruction::Add { dst, src }
            | Instruction::Sub { dst, src } => {
                bytes.push(arena, enc_reg_pair(dst, src));
            }
            Instruction::MovImm { dst, imm }
            | Instruction::AddImm { dst, imm }
            | Instruction::SubImm { dst, imm } => {
                bytes.push(arena, enc_reg_single(dst));
                bytes.extend_from_slice(arena, &enc_u32(imm));
            }

            Instruction::Call { tgt } => {
                bytes.extend_from_slice(arena, &enc_u32(tgt));
            }
            Instruction::Return => {}

            Instruction::JumpEQ { lhs, rhs, tgt }
            | Instruction::JumpNE { lhs, rhs, tgt }
            | Instruction::JumpLT { lhs, rhs, tgt }
            | Instruction::JumpLE { lhs, rhs, tgt }
            | Instruction::JumpGT { lhs, rhs, tgt }
            | Instruction::JumpGE { lhs, rhs, tgt } => {
                bytes.push(arena, enc_reg_pair(lhs, rhs));
                bytes.extend_from_slice(arena, &enc_u32(tgt));
            }

            Instruction::JumpIfEndOfLine { tgt } => {
                bytes.extend_from_slice(arena, &enc_u32(tgt));
            }
            Instruction::JumpIfMatchCharset { idx, min, max, tgt } => {
                bytes.extend_from_slice(arena, &enc_u32(idx));
                bytes.extend_from_slice(arena, &enc_u32(min));
                bytes.extend_from_slice(arena, &enc_u32(max));
                bytes.extend_from_slice(arena, &enc_u32(tgt));
            }
            Instruction::JumpIfMatchPrefix { idx, tgt }
            | Instruction::JumpIfMatchPrefixInsensitive { idx, tgt } => {
                bytes.extend_from_slice(arena, &enc_u32(idx));
                bytes.extend_from_slice(arena, &enc_u32(tgt));
            }

            Instruction::FlushHighlight { kind } => {
                bytes.push(arena, enc_reg_single(kind));
            }
            Instruction::AwaitInput => {}
        }

        bytes
    }

    pub fn decode(bytes: &[u8]) -> (Option<Self>, usize) {
        let mut pc = 0;
        let instr = instruction_decode!(bytes, pc, {
            Mov { dst, src } => {
                Instruction::Mov { dst, src }
            }
            Add { dst, src } => {
                Instruction::Add { dst, src }
            }
            Sub { dst, src } => {
                Instruction::Sub { dst, src }
            }
            MovImm { dst, imm } => {
                Instruction::MovImm { dst, imm }
            }
            AddImm { dst, imm } => {
                Instruction::AddImm { dst, imm }
            }
            SubImm { dst, imm } => {
                Instruction::SubImm { dst, imm }
            }
            Call { tgt } => {
                Instruction::Call { tgt }
            }
            Return => {
                Instruction::Return
            }
            JumpEQ { lhs, rhs, tgt } => {
                Instruction::JumpEQ { lhs, rhs, tgt }
            }
            JumpNE { lhs, rhs, tgt } => {
                Instruction::JumpNE { lhs, rhs, tgt }
            }
            JumpLT { lhs, rhs, tgt } => {
                Instruction::JumpLT { lhs, rhs, tgt }
            }
            JumpLE { lhs, rhs, tgt } => {
                Instruction::JumpLE { lhs, rhs, tgt }
            }
            JumpGT { lhs, rhs, tgt } => {
                Instruction::JumpGT { lhs, rhs, tgt }
            }
            JumpGE { lhs, rhs, tgt } => {
                Instruction::JumpGE { lhs, rhs, tgt }
            }
            JumpIfEndOfLine { tgt }=> {
                Instruction::JumpIfEndOfLine { tgt }
            }
            JumpIfMatchCharset { idx, min, max, tgt } => {
                Instruction::JumpIfMatchCharset { idx, min, max, tgt }
            }
            JumpIfMatchPrefix { idx, tgt } => {
                Instruction::JumpIfMatchPrefix { idx, tgt }
            }
            JumpIfMatchPrefixInsensitive { idx, tgt } => {
                Instruction::JumpIfMatchPrefixInsensitive { idx, tgt }
            }
            FlushHighlight { kind } => {
                Instruction::FlushHighlight { kind }
            }
            AwaitInput=> {
                Instruction::AwaitInput
            }
            _ => return (None, 1),
        });
        (Some(instr), pc)
    }

    pub fn mnemonic<'a>(&self, arena: &'a Arena, config: &MnemonicFormattingConfig) -> BString<'a> {
        let mut str = BString::empty();
        let _i = config.instruction_prefix;
        let i_ = config.instruction_suffix;
        let _r = config.register_prefix;
        let r_ = config.register_suffix;
        let _a = config.address_prefix;
        let a_ = config.address_suffix;
        let _n = config.numeric_prefix;
        let n_ = config.numeric_suffix;

        match *self {
            Instruction::Mov { dst, src } => {
                arena_write_fmt!(arena, str, "{_i}mov{i_}    {_r}{dst}{r_}, {_r}{src}{r_}");
            }
            Instruction::Add { dst, src } => {
                arena_write_fmt!(arena, str, "{_i}add{i_}    {_r}{dst}{r_}, {_r}{src}{r_}");
            }
            Instruction::Sub { dst, src } => {
                arena_write_fmt!(arena, str, "{_i}sub{i_}    {_r}{dst}{r_}, {_r}{src}{r_}");
            }
            Instruction::MovImm { dst, imm } => {
                if dst == Register::ProgramCounter {
                    arena_write_fmt!(arena, str, "{_i}movi{i_}   {_r}{dst}{r_}, {_a}{imm}{a_}");
                } else {
                    arena_write_fmt!(arena, str, "{_i}movi{i_}   {_r}{dst}{r_}, {_n}{imm}{n_}");
                }
            }
            Instruction::AddImm { dst, imm } => {
                arena_write_fmt!(arena, str, "{_i}addi{i_}   {_r}{dst}{r_}, {_n}{imm}{n_}");
            }
            Instruction::SubImm { dst, imm } => {
                arena_write_fmt!(arena, str, "{_i}subi{i_}   {_r}{dst}{r_}, {_n}{imm}{n_}");
            }

            Instruction::Call { tgt } => {
                arena_write_fmt!(arena, str, "{_i}call{i_}   {_a}{tgt}{a_}");
            }
            Instruction::Return => {
                arena_write_fmt!(arena, str, "{_i}ret{i_}");
            }

            Instruction::JumpEQ { lhs, rhs, tgt } => {
                arena_write_fmt!(
                    arena,
                    str,
                    "{_i}jeq{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}"
                );
            }
            Instruction::JumpNE { lhs, rhs, tgt } => {
                arena_write_fmt!(
                    arena,
                    str,
                    "{_i}jne{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}"
                );
            }
            Instruction::JumpLT { lhs, rhs, tgt } => {
                arena_write_fmt!(
                    arena,
                    str,
                    "{_i}jlt{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}"
                );
            }
            Instruction::JumpLE { lhs, rhs, tgt } => {
                arena_write_fmt!(
                    arena,
                    str,
                    "{_i}jle{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}"
                );
            }
            Instruction::JumpGT { lhs, rhs, tgt } => {
                arena_write_fmt!(
                    arena,
                    str,
                    "{_i}jgt{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}"
                );
            }
            Instruction::JumpGE { lhs, rhs, tgt } => {
                arena_write_fmt!(
                    arena,
                    str,
                    "{_i}jge{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}"
                );
            }

            Instruction::JumpIfEndOfLine { tgt } => {
                arena_write_fmt!(arena, str, "{_i}jeol{i_}   {_a}{tgt}{a_}");
            }
            Instruction::JumpIfMatchCharset { idx, min, max, tgt } => {
                arena_write_fmt!(
                    arena,
                    str,
                    "{_i}jc{i_}     {_n}{idx}{n_}, {_n}{min}{n_}, {_n}{max}{n_}, {_a}{tgt}{a_}"
                );
            }
            Instruction::JumpIfMatchPrefix { idx, tgt } => {
                arena_write_fmt!(arena, str, "{_i}jp{i_}     {_n}{idx}{n_}, {_a}{tgt}{a_}");
            }
            Instruction::JumpIfMatchPrefixInsensitive { idx, tgt } => {
                arena_write_fmt!(arena, str, "{_i}jpi{i_}    {_n}{idx}{n_}, {_a}{tgt}{a_}");
            }

            Instruction::FlushHighlight { kind } => {
                arena_write_fmt!(arena, str, "{_i}flush{i_}  {_r}{kind}{r_}");
            }
            Instruction::AwaitInput => {
                arena_write_fmt!(arena, str, "{_i}await{i_}");
            }
        }

        str
    }
}

#[derive(Default)]
pub struct MnemonicFormattingConfig<'a> {
    // Color used for highlighting the instruction.
    pub instruction_prefix: &'a str,
    pub instruction_suffix: &'a str,

    // Color used for highlighting a register name.
    pub register_prefix: &'a str,
    pub register_suffix: &'a str,

    // Color used for highlighting an immediate value.
    pub address_prefix: &'a str,
    pub address_suffix: &'a str,

    // Color used for highlighting an immediate value.
    pub numeric_prefix: &'a str,
    pub numeric_suffix: &'a str,
}
