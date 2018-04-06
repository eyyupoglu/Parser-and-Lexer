#r "FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
open Microsoft.FSharp.Text.Lexing
open System

#load "AST.fs"
open AST

#load "parser.fs"
open parser
#load "lexer.fs"
open lexer



let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = parser.start lexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let e = parse (Console.ReadLine())

printfn "%A" (e)


let rec edge start endd act = 
    String.Format("q%d ->  q%d [label = \"%s\"];", start endd act)

let rec arithm = function
    | N (int) -> string int 
    | X str -> string str
    | Plus (a , b) -> arithm a + "+" + arithm b
    | Minus (a, b) -> arithm a + "-" + arithm b
    | Multiplication (a, b) -> arithm a + "*" + arithm b
    | Power (a, b) -> arithm a + "^" + arithm b
    | UPlus a -> "+" + arithm a 
    | UMinus a -> "-" + arithm a
    | _ -> "error"

let rec bool = function
    | True -> string true
    | False -> string false
    | And  (a , b) ->   (bool a) +  (bool b) 
    | Or  (a, b)  -> "(" +  (bool a) + "|"+ (bool b) + ")"
    | CAnd  (a,  b) -> "(" +  (bool a) + "&&" + (bool b) + ")"
    | COr  (a,  b) -> "(" +  (bool a) + "||" + (bool b) + ")"
    | Not  b -> "!"+ (bool b)
    | Equals  (first , second) -> (arithm first) + "=" + (arithm second)
    | Unequal  ( a , b ) -> arithm a + "!=" + arithm b
    | Greater ( a , b ) -> arithm a + ">" + arithm b
    | GreaterOrEqual ( a , b ) -> arithm a + ">=" + arithm b
    | Smaller ( a , b ) -> arithm a + "<" + arithm b
    | SmallerOrEqual ( a , b ) -> arithm a + "<=" + arithm b
    | Nested b -> bool b 


let rec compile s e = function
    | Assign ( str, num ) -> "q" + (string s) + "  ->  " 
                            + "q" + (string (s+1)) + "[label = \"" 
                            + str + ":=" + (arithm num) + "]"
    | Skip ->         "q" +  (string s)     
    | Command (c1, c2 ) ->  compile (s+1) e c1 + "\n" + compile (s+2) e c2
    | If (gc) -> compile2 (s+1) e gc
    | Do (gc) ->   compile2 (s) e gc 
and compile2 s e = function
    | SingletonGuarded (b, c) -> 
         "q" + (string s) + "  ->  "  + "q" + (string (s+1)) + "[label = \""  + (string (bool b)) + "]" + "\n" + compile (s+1) e c 
    | Associate (gc1 ,gc2 ) -> compile2 (s+1) e gc1 + "\n" + compile2 (s+2) e gc2 



let rec repeat compile s e asd = function
    | 0 -> ""
    | n -> repeat compile s e asd (n-1)

printfn "%A" (compile -1 0 (e))
(*let rec compute =
    try
    let e = parse (Console.ReadLine())
    printfn "ok"
    

    with err -> printfn "ko"

// Start interacting with the user
compute
*)