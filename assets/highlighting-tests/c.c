// Comments
// Single-line comment

/*
 * Multi-line
 * comment
 */

#include <stdio.h>
#include <stdbool.h>
#define MAX_BUFFER 1024
#ifndef MY_HEADER_H
#define MY_HEADER_H
#endif

// Numbers
42;
3.14;
.5;
1e10;
1.5e-3;
0xff;
0xFF;
0b1010;
0o77;
42ul;
3.14f;

// Constants
true;
false;
NULL;

// Strings and characters
'a';
'\n';
"double quotes with escape: \" \n \t \\";

// Control flow keywords
int main(int argc, char *argv[]) {
    if (argc > 1) {
        // ...
    } else if (argc == 0) {
        // ...
    } else {
        // ...
    }

    for (int i = 0; i < 10; i++) {
        if (i == 5) continue;
        if (i == 8) break;
    }

    while (false) { }
    do { } while (false);

    switch (argc) {
        case 1:
            break;
        default:
            goto end;
    }

end:
    return 0;
}

// Other keywords and structures
struct Point {
    int x;
    int y;
};

typedef union {
    char c;
    double d;
} DataUnion;

enum Color { RED, GREEN, BLUE };

extern int global_var;
static const int MAX_RETRIES = 5;
volatile int hardware_register;
register int fast_loop_counter;

// C99/C11 specific keywords
_Atomic int atomic_counter;
_Bool is_active;
_Noreturn void die(void);

// Function calls
printf("hello\n");
sizeof(struct Point);
