#r "FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
open Microsoft.FSharp.Text.Lexing
open System

#load "AST.fs"
open AST

#load "parser.fs"
open parser
#load "lexer.fs"
open lexer


// Parse text into abstract syntax tree.
let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = parser.start lexer.tokenize lexbuf
    res

// Take text from console and determine whether it conforms to the GCL grammar.
let rec check =
    try
    let e = parse (Console.ReadLine())
    printfn "ok"

    with err -> printfn "ko"

// Start interacting with the user
check