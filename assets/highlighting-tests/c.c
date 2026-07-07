// Line comment
/* Block comment
   spanning lines */

#include <stdio.h>
#include <stdint.h>
#define MAX 100
#ifndef GUARD_H
#define GUARD_H

typedef struct Point {
    double x;
    double y;
} Point;

enum Color { RED, GREEN, BLUE };

static const char *NAME = "edit";

void numbers(void) {
    int a = 42;
    unsigned long b = 0xffUL;
    int c = 0b1010;
    double d = 1.5e-3;
    float f = 3.14f;
    long long g = 1000000LL;
}

void strings(void) {
    const char *s = "double \" quote \n escape";
    char ch = 'a';
    char nl = '\n';
    wchar_t w = L'x';
}

int control(int n) {
    for (int i = 0; i < n; i++) {
        if (i == 5) continue;
        switch (i) {
            case 1: break;
            default: break;
        }
    }
    while (n > 0) { n--; }
    return n;
}

int main(int argc, char **argv) {
    Point p = { .x = 1.0, .y = 2.0 };
    _Bool flag = true;
    printf("%f %d\n", p.x, flag);
    if (argv == NULL) return 1;
    return 0;
}

#endif
