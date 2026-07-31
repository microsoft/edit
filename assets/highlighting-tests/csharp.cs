// Single-line comment
/* Block comment
   spanning multiple lines */

#nullable enable
#region Usings
global using static System.Console;
using System;
using System.Collections.Generic;
#endregion

#if DEBUG
#pragma warning disable CS1591
#endif

namespace Demo.App;

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
0XFF;
0b1010;
0B1010;
42u;
42U;
42L;
42UL;
42ul;
42Ul;
42lu;
3.14f;
3.14d;
3.14m;
3.14M;
-.5_0_1e3_0d;
-.5_0_1E3_0D;

// Constants
true;
false;
null;

// Strings and characters
'a';
'\n';
"double quotes with escape: \" \n \t \\";
$"double quotes with a value {1 + 1}";
@"C:\Temp\file.txt";
@"verbatim with ""escaped"" quotes";
$@"interpolated verbatim {1}";
@$"interpolated verbatim {1}";
"""
raw string literal
""";

[Obsolete("Use NewThing instead")]
public sealed class Greeter
{
    private readonly string _name;

    public Greeter(string name)
    {
        _name = name;
    }

    public string SayHello(int count = 3)
    {
        var list = new List<int> { 1, 2, 3 };
        var pi = 3.14159;
        var hex = 0xDEAD_BEEF;
        return $"Hello {_name}, count={count}";
    }
}

public record Person(string Name, int Age);

class Animal
{
    private int _age;
    public int Age { get => _age; init => _age = value; }
    protected bool _isAlive;
    public bool IsAlive => _isAlive;

    public virtual void Speak() { }
}

class Dog : Animal
{
    private string _bark = "woof";

    public override void Speak()
    {
        Console.WriteLine(_bark);
    }
}

public static class Program
{
    public static int Main(string[] args)
    {
        // Control flow keywords
        if (args.Length == 0)
        {
            return 1;
        }
        else if (args.Length > 10)
        {
            return 2;
        }
        else
        {
            for (int i = 0; i < 10; i++)
            {
                if (i == 5) continue;
                if (i == 8) break;
            }

            foreach (var arg in args)
            {
                Console.WriteLine(arg);
            }

            while (false) { }
            do { } while (true);

            switch (args.Length)
            {
                case 1: break;
                default: break;
            }
        }

        try
        {
            throw new Exception("oops");
        }
        catch (Exception e) when (e is InvalidOperationException)
        {
        }
        finally
        {
        }

        var query = from a in args where a is not null orderby a select a;
        var g = new Greeter(args[0]);
        Console.WriteLine(g.SayHello());
        return 0;
    }
}
