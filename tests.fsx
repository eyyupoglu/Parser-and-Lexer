
#r "myLang/FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
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

(* Take text from console and determine whether it conforms to
*  the GCL grammar.
*)
let rec check input =
    try
    let e = parse input
    "ok"

    with err -> "ko"

let test1 = "ok" = (check "if  x != x -> x := x [] x > x -> y:=z;
do x = x -> z:=q od fi")
let test2 = "ko" = check "if 
x != x -> x := x
[] x > x -> y:=z do x = x -> z:=q od  // missing semicolon 
fi"

let test3 = "ok" = check "y:=1;
do x>0 -> y:=x*y;
          x:=x-1
od"
(* The last test is failing and we are aware of that, we will
 * improve in following weeks. We ran out of time.
*)
let print = function
    | true -> printfn "SUCCESS"
    | false -> printfn "FAIL"
    

print test1
print test2
print test3
