// Comments
// Single-line comment

/*
Multi-line
comment
*/

global using static System.Console;
using System.Diagnostics;
using System;

// Numbers
42;
3.14;
.5;
10_000_000;
1e10;
1.5e-3;
0xff;
0xffu;
0xffl;
0xfful;
0xFF;
0Xff;
0XFF;
0XFFU;
0XFFL;
0XFFLU;
0b1010;
0B1010;
42u;
42U;
42l;
42L;
42UL;
42Ul;
42uL;
42ul;
42LU;
42Lu;
42lU;
42lu;
3.14f;
3.14d;

// Constants
true;
false;
null;

// Strings and Characters
'a';
'\n';
"double quotes with escape: \" \n \t \\";
$"";
@"";
$@"";
@$"";

// Control flow keywords
if (true)
{

}
else if (false)
{

}
else
{

}

for (int i = 0; i < 10; i++)
{
    if (i == 5) continue;
    if (i == 8) break;
}

while (false) { }
do { } while (true);

switch (42)
{
    case 1: break;
    default: break;
}

try
{
    throw new Exception("oops");
}
catch (System.Exception)
{

}
finally
{

}

Debug;

// Other keywords (some are contextually reserved)
var a = 1;
dynamic b = 2;
T c = 3;

void Greet(string name)
{
    return "Hello, " + name;
}

static void Greet(string name)
{
    return "Hello, " + name;
}

async void Greet(string name)
{
    return "Hello, " + name;
}

class Animal
{
    private int age;
    protected bool isAlive;
    public Animal()
}
