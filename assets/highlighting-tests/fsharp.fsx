// Single-line comment
/// Documentation comment
(* Block comment
   spanning multiple lines *)
(*** Banner comment ***)

#r "nuget: Newtonsoft.Json, 13.0.3"
#load "some-script.fsx"
#I "lib"
#nowarn "40"
#if DEBUG
#light
#endif

module Demo.App

open System

// Numbers
42
3.14
.5
10_000_000
1e10
1.5e-3
0xff
0xFF
0b1010
0o777
42y
42uy
42s
42us
42L
42UL
42n
42un
42I
3.14f
3.14m
3.14M
-.5_0_1e3_0

// Constants
true
false
null

// Strings and characters
'a'
'\n'
'\\'
'\''
"double quotes with escape: \" \n \t \\"
$"interpolated {1 + 1}"
@"C:\Temp\file.txt"
@"verbatim with ""escaped"" quotes"
$@"interpolated verbatim {1}"
"""
triple-quoted string
"""
$"""interpolated triple-quoted {1}"""

// Backtick identifiers
let ``an identifier with spaces`` = 1

[<Obsolete("Use NewThing instead")>]
type Person(name: string, age: int) =
    member val Nickname = "" with get, set
    member _.Name: string = name
    member _.Age: int = age
    override _.ToString() = $"{name} ({age})"

[<AbstractClass>]
type Shape() =
    abstract member Area: unit -> float
    default _.Area() = 0.0

type Color =
    | Red
    | Green
    | Blue

type IGreeter =
    abstract member Greet: string -> unit

// Generic type parameters must not be mistaken for character literals
let swap (a: 'a, b: 'b) = (b, a)
let inline add (x: ^T) (y: ^T) = x + y

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

let demoLoops () =
    let mutable counter = 0

    for i = 1 to 3 do
        counter <- counter + i

    for i = 3 downto 1 do
        counter <- counter - i

    while counter < 20 do
        counter <- counter + 1

    counter

let demoLambdas () =
    let twice = fun x -> x * 2
    let thrice = function x -> x * 3
    [ 1; 2; 3 ] |> List.map twice |> List.map thrice

let demoResources () =
    use stream = new IO.MemoryStream()
    assert (stream.Length = 0L)
    lazy (stream.Length)

exception MyError of string

module private Internals =
    let helper () = raise (MyError "boom")

[<EntryPoint>]
let main argv =
    printfn "%s" (Person("Ada", 37).ToString())
    demoLoops () |> ignore
    demoLambdas () |> ignore
    0
