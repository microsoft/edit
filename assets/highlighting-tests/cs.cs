// Comments
// Single-line comment

/*
Multi-line
comment
*/

global using static System.Console;
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
-.5_0_1e3_0d;

// Constants
true;
false;
null;

// Strings and Characters
'a';
'\n';
"double quotes with escape: \" \n \t \\";
$"double quotes with a value {1 + 1}";
@"double quotes with ";
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

// Other keywords (some are contextually reserved)
var a = 1;
dynamic b = 2;
T c = 3;

string Greet(string name)
{
    return "Hello, " + name;
}

static int Factorial(int n)
{
    if (n == 0)
    {
        return 1;
    }

    return n * Factorial(n - 1);
}

class Animal
{
    private int _Age;
    public int Age { get => _Age; }
    protected bool _IsAlive;
    public bool IsAlive { get => _IsAlive; }
    public Animal(int age, bool isAlive)
    {
        _Age = age;
        _IsAlive = isAlive;
    }

    public virtual void Speak() { }
}

class Dog : Animal
{
    private string _Bark;

    public Dog(string bark, int age, bool isAlive) : base(age, isAlive)
    {

    }

    public override void Speak()
    {
        Console.WriteLine(_Bark);
    }
}
