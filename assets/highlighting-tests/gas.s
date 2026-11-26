# Single-line comment
/*
 * Multi-line
 * comment
 */

// Preprocessor directives
#include <avr/io.h>
#define LED_PIN 5
#ifndef F_CPU
#  define F_CPU 16000000
#endif
%define BUFSIZE 64

// Directives
.section .text
.global main
.type main, @function
.equ BAUD, 9600
.set COUNT, 10
LIMIT = 100

// Labels
main:
.Lloop:
1:      nop
2: 3:   nop
start:  nop
        rjmp 1b
        rjmp 1f

// Numbers
.byte 42, 0x2A, 0X2a, 0b101010, 052, -1
.float 1.5, 2.5e3
.byte 1x, 0x, 9a

// Strings and characters
.ascii "escapes: \" \\ \n \t"
.asciz "unterminated
.byte 'A', '\n', 'B

// Macros
.macro push_pair a, b
        push \a
        push \b
.endm
        push_pair r16, r17

// Instructions
        ldi r16, 0xFF           ; all pins are outputs
        out _SFR_IO_ADDR(DDRB), r16
        ldi ZL, lo8(table)
        ldi ZH, hi8(table)
        ld r24, X+
        ldd r25, Y+1
        st -Z, r0
        sbi _SFR_IO_ADDR(PORTB), LED_PIN
delay:  dec r18
        brne delay
        ret

        movq $60, %rax          # exit
        leaq msg(%rip), %rsi
        movl arr(,%rcx,4), %eax
        movl -4(%rbp), %eax
        call puts@PLT
        rep movsb

.intel_syntax noprefix
        mov eax, dword ptr [rbp - 4]
        lea rsi, [rip + msg]
        movzx ecx, byte ptr [rsi + rcx*2]
        mov rax, qword ptr fs:[0x28]
        mov r8d, 0FFh
        vmovdqu ymm0, ymmword ptr [rdi]
        mov rdi, offset msg
.att_syntax

        push {ip, lr}
        mov r0, #5
        ldr r1, =0x20000000     @ SRAM base
        add r0, r0, #-1
        bx lr

        mov x0, #0
        ldr w1, [x0, #8]
        ldr b0, [x0]
        b.ne 1b
        str xzr, [sp, #-16]!

        addi a0, a0, 1
        fadd.s fa0, fa1, ft0
        lui t0, %hi(msg)
        addi t0, t0, %lo(msg)
        sw zero, 0(sp)
