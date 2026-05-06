"use strict";

const MAX_U32 = 0xffffffff;
const encoder = new TextEncoder();

function readU32(view, off) {
    return view.getUint32(off, true);
}

function saturatingAdd(lhs, rhs) {
    const value = lhs + rhs;
    return value > MAX_U32 ? MAX_U32 : value;
}

function saturatingSub(lhs, rhs) {
    return lhs < rhs ? 0 : lhs - rhs;
}

function asciiLower(byte) {
    return byte >= 0x41 && byte <= 0x5a ? byte + 0x20 : byte;
}

function inSet(bitmap, byte) {
    const loNibble = byte & 0xf;
    const hiNibble = byte >>> 4;
    return (bitmap[loNibble] & (1 << hiNibble)) !== 0;
}

function charsetGobble(bytes, off, bitmap, min, max) {
    let i = 0;
    while (i < max) {
        const idx = off + i;
        if (idx >= bytes.length || !inSet(bitmap, bytes[idx])) {
            break;
        }
        i += 1;
    }
    return i >= min ? off + i : -1;
}

function matchPrefix(bytes, off, needle) {
    if (off >= bytes.length || bytes.length - off < needle.length) {
        return false;
    }
    for (let i = 0; i < needle.length; i += 1) {
        if (bytes[off + i] !== needle[i]) {
            return false;
        }
    }
    return true;
}

function matchPrefixInsensitive(bytes, off, needle) {
    if (off >= bytes.length || bytes.length - off < needle.length) {
        return false;
    }
    for (let i = 0; i < needle.length; i += 1) {
        if (asciiLower(bytes[off + i]) !== needle[i]) {
            return false;
        }
    }
    return true;
}

class Runtime {
    constructor(assembly, strings, charsets, entrypoint) {
        this.assembly = assembly;
        this.view = new DataView(assembly.buffer, assembly.byteOffset, assembly.byteLength);
        this.strings = strings;
        this.charsets = charsets;
        this.entrypoint = entrypoint >>> 0;
        this.stack = [];
        this.registers = new Uint32Array(16);
        this.registers[2] = this.entrypoint;
    }

    snapshot() {
        return {
            stack: this.stack.slice(),
            registers: new Uint32Array(this.registers),
        };
    }

    restore(state) {
        this.stack = state.stack.slice();
        this.registers.set(state.registers);
    }

    saveRegisters() {
        for (let i = 2; i < 16; i += 1) {
            this.stack.push(this.registers[i]);
        }
    }

    loadRegisters() {
        if (this.stack.length < 14) {
            return false;
        }
        const start = this.stack.length - 14;
        for (let i = 0; i < 14; i += 1) {
            this.registers[i + 2] = this.stack[start + i];
        }
        this.stack.length = start;
        return true;
    }

    parseNextLine(bytes) {
        const result = [0, 0];
        const registers = this.registers;
        const assembly = this.assembly;
        const view = this.view;
        const strings = this.strings;
        const charsets = this.charsets;

        registers[0] = 0;
        registers[1] = 0;

        for (; ;) {
            const pc = registers[2];
            const opcode = assembly[pc];

            switch (opcode) {
                case 0: {
                    registers[2] += 2;
                    const pair = assembly[pc + 1];
                    registers[pair & 0xf] = registers[pair >>> 4];
                    break;
                }
                case 1: {
                    registers[2] += 2;
                    const pair = assembly[pc + 1];
                    const dst = pair & 0xf;
                    registers[dst] = saturatingAdd(registers[dst], registers[pair >>> 4]);
                    break;
                }
                case 2: {
                    registers[2] += 2;
                    const pair = assembly[pc + 1];
                    const dst = pair & 0xf;
                    registers[dst] = saturatingSub(registers[dst], registers[pair >>> 4]);
                    break;
                }
                case 3: {
                    registers[2] += 6;
                    registers[assembly[pc + 1] & 0xf] = readU32(view, pc + 2);
                    break;
                }
                case 4: {
                    registers[2] += 6;
                    const dst = assembly[pc + 1] & 0xf;
                    registers[dst] = saturatingAdd(registers[dst], readU32(view, pc + 2));
                    break;
                }
                case 5: {
                    registers[2] += 6;
                    const dst = assembly[pc + 1] & 0xf;
                    registers[dst] = saturatingSub(registers[dst], readU32(view, pc + 2));
                    break;
                }
                case 6: {
                    registers[2] += 5;
                    const target = readU32(view, pc + 1);
                    this.saveRegisters();
                    registers[2] = target;
                    break;
                }
                case 7: {
                    registers[2] += 1;
                    if (!this.loadRegisters()) {
                        registers.fill(0);
                        registers[2] = this.entrypoint;
                        if (result[result.length - 2] < bytes.length) {
                            result.push(bytes.length, 0);
                        }
                        return result;
                    }
                    break;
                }
                case 8:
                case 9:
                case 10:
                case 11:
                case 12:
                case 13: {
                    registers[2] += 6;
                    const pair = assembly[pc + 1];
                    const lhs = registers[pair & 0xf];
                    const rhs = registers[pair >>> 4];
                    const target = readU32(view, pc + 2);
                    let jump;
                    switch (opcode) {
                        case 8:
                            jump = lhs === rhs;
                            break;
                        case 9:
                            jump = lhs !== rhs;
                            break;
                        case 10:
                            jump = lhs < rhs;
                            break;
                        case 11:
                            jump = lhs <= rhs;
                            break;
                        case 12:
                            jump = lhs > rhs;
                            break;
                        default:
                            jump = lhs >= rhs;
                            break;
                    }
                    if (jump) {
                        registers[2] = target;
                    }
                    break;
                }
                case 14: {
                    registers[2] += 5;
                    if (registers[0] >= bytes.length) {
                        registers[2] = readU32(view, pc + 1);
                    }
                    break;
                }
                case 15: {
                    registers[2] += 17;
                    const idx = readU32(view, pc + 1);
                    const min = readU32(view, pc + 5);
                    const max = readU32(view, pc + 9);
                    const target = readU32(view, pc + 13);
                    const next = charsetGobble(bytes, registers[0], charsets[idx], min, max);
                    if (next >= 0) {
                        registers[0] = next;
                        registers[2] = target;
                    }
                    break;
                }
                case 16: {
                    registers[2] += 9;
                    const idx = readU32(view, pc + 1);
                    const target = readU32(view, pc + 5);
                    const needle = strings[idx];
                    if (matchPrefix(bytes, registers[0], needle)) {
                        registers[0] += needle.length;
                        registers[2] = target;
                    }
                    break;
                }
                case 17: {
                    registers[2] += 9;
                    const idx = readU32(view, pc + 1);
                    const target = readU32(view, pc + 5);
                    const needle = strings[idx];
                    if (matchPrefixInsensitive(bytes, registers[0], needle)) {
                        registers[0] += needle.length;
                        registers[2] = target;
                    }
                    break;
                }
                case 18: {
                    registers[2] += 2;
                    const kind = registers[assembly[pc + 1] & 0xf];
                    const start = Math.min(registers[1], bytes.length);
                    const lastStartIndex = result.length - 2;
                    const lastKindIndex = result.length - 1;
                    if (result[lastStartIndex] === start || result[lastKindIndex] === kind) {
                        result[lastKindIndex] = kind;
                    } else {
                        result.push(start, kind);
                    }
                    registers[1] = registers[0];
                    break;
                }
                case 19: {
                    registers[2] += 1;
                    if (registers[0] >= bytes.length) {
                        if (result[result.length - 2] < bytes.length) {
                            result.push(bytes.length, 0);
                        }
                        return result;
                    }
                    break;
                }
                default:
                    throw new Error(`Invalid LSH opcode ${opcode} at ${pc}`);
            }
        }
    }
}

function encodeString(value) {
    return encoder.encode(value);
}
