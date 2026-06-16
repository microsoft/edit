// Comments
// Single-line comment

/*
 * Multi-line
 * comment
 */

// Preprocessor directives

#include <stdio.h>
#include "foo.h"
#ifndef HEADER_GUARD
#define HEADER_GUARD
#endif
#define MACRO(arg) (void)arg;

// Numbers
42;
3.14;
3.14f;
.5;
1e10;
-12L;
15lU;
0xff;
0xFF;
0b1010;
0o77;

// Constants
true;
false;
NULL;

// Strings
"double quotes with escape: \" \n \t \\";

// Control flow keywords
if (true) {
} else if (false) {
} else {
}

for (int i = 0; i < 10; i++) {
	if (i == 5)
		continue;
	if (i == 8)
		break;
}

while (false) {
}
do {
} while (false);

switch (42) {
case 1:
	break;
default:
	break;
}

goto cleanup;

cleanup :

	int a = 1;
signed long long b = 2;
unsigned short int c = 3;
float pi = 3.14f;
double tau = 6.283F;
short foo = 42;
char c = 'c';
char newline = '\n';
const char *text = "Hello World!";

int add(int a, int b)
{
	return a + b;
}

void greet(const char *name)
{
	printf("Hello %s!\n", name);
}

static inline do_nothing()
{
	static int a = 3;
	volatile long long b = 4;
}

typedef struct {
	char r;
	char g;
	char b;
	char a;
} Color;

Color cornflower = { 100, 149, 237, 255 };
Color blue = { .b = 255, .a = 255 };

enum EntityKind {
	None = 0,
	Bear,
	Bee,
	Dog,
	Cat,
};

(void)0;

int main(int argc, char *argv[])
{
	greet("lhecker");
	return 0;
}
