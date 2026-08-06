// Single-line comment
/*
 * Multi-line
 * comment
 */

// Preprocessor directives
#include <stdio.h>
#include "foo.h"
#ifndef HEADER_GUARD
#  define HEADER_GUARD
#endif
#define MAX_BUFFER 1024
#define MIN(a, b) \
    ((a) < (b) ? (a) : (b))
#pragma once
#warning "unmaintained"

// Numbers
42;
0777;
3.14;
.5;
1e10;
1.5e-3;
0xff;
0b1010;
0x1p-3;
-12L;
15lU;
6.283F;
1'000'000;

// Constants and literals
true;
false;
NULL;
nullptr;
'a';
'\'';
"escapes: \" \n \t \\";

// Control flow
int main(int argc, char *argv[])
{
    for (int i = 0; i < 10; i++) {
        if (i == 5) {
            continue;
        } else if (i == 8) {
            break;
        } else {
            goto end;
        }
    }

    do {
        printf("Hello %s!\n", argv[0]);
    } while (false);

    switch (argc) {
    case 1:
        break;
    default:
        break;
    }

end:
    return 0;
}

// Types and storage classes
extern int global_var;
static const char *text = "Hello World!";
volatile unsigned short hardware_register;
register signed long long fast_loop_counter;
_Atomic int atomic_counter;
_Bool is_active;
thread_local double tau;
typeof(tau) tau_copy;

// Aggregates
struct Point {
    int x;
    int y;
};

typedef union {
    char bytes[4];
    float value;
} Pixel;

enum EntityKind {
    Bear,
    Bee,
    Dog,
};

// Designated initializers, where `.x` must not be mistaken for a number.
struct Point origin = { .x = 0, .y = 0 };

// Declarations and calls
_Noreturn void die(void);
alignas(16) char buffer[64];
static_assert(sizeof(struct Point) == 8, "unexpected layout");

static inline int add(int a, int b)
{
    return a + b;
}
