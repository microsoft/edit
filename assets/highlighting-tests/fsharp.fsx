// --- F# highlighting test ---
// Exercises: comments, block comments, strings, chars, numbers, directives,
// keywords/control flow, types/members, and backtick identifiers.

#r "System.Runtime"
#r "nuget: Newtonsoft.Json, 13.0.3"
#load "some-script.fsx"

(*** Block comment (not nested in this highlighter) ***)
(* Another block comment *)

open System
open Newtonsoft.Json

module ``My Module With Spaces`` =
    let pi = 3.14159
    let hex = 0xDEAD_BEEF
    let bin = 0b1010_1100
    let sci = 1.23e-4
    let mutable counter = 0

    let name = "Edit"
    let path = @"C:\Program Files\Edit\edit.exe"
    let escaped = "tab:\t newline:\n quote:\" backslash:\\"
    let interpolated = $"Hello, {name}!"
    let interpolatedVerbatim = $@"Path: {path}"

    let triple = """
This is a triple-quoted string.
It can span multiple lines without escapes.
"""

    let chA = 'a'
    let chN = '\n'

    let rec fib n =
        if n <= 1 then n
        else fib (n - 1) + fib (n - 2)

    let classify x =
        match x with
        | 0 -> "zero"
        | 1 | 2 -> "small"
        | _ when x < 0 -> "negative"
        | _ -> "other"

    let tryParseInt (s: string) =
        try
            let v = Int32.Parse(s)
            Ok v
        with
        | :? FormatException -> Error "format"
        | ex -> Error ex.Message

    type Person(name: string, age: int) =
        member _.Name = name
        member _.Age = age
        override _.ToString() = $"{name} ({age})"

    let demoLoops () =
        for i = 1 to 3 do
            counter <- counter + i

        while counter < 20 do
            counter <- counter + 1

        // Method-call style tokenization
        let p = Person("Ada", 37)
        printfn "%s" (p.ToString())

    // Single-backtick identifier fallback (not typical F#, but supported by lexer)
    let `singleBacktickIdentifier` = 42

    demoLoops ()
