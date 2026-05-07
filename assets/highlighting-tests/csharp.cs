// Single-line comment
/* Block comment
   spanning multiple lines */

using System;
using System.Collections.Generic;

#nullable enable

namespace Demo.App;

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

        // Regular string
        var a = "Hello, \"world\"";

        // Verbatim string
        var b = @"C:\\Temp\\file.txt";

        // Interpolated string
        var c = $"Hello {_name}, count={count}";

        // Interpolated verbatim string
        var d = $@"Name={_name}\nCount={count}";

        return c;
    }
}

public record Person(string Name, int Age);

public static class Program
{
    public static int Main(string[] args)
    {
        if (args.Length == 0)
        {
            return 1;
        }

        var g = new Greeter(args[0]);
        Console.WriteLine(g.SayHello());
        return 0;
    }
}
