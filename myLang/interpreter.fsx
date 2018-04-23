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
//input
printfn "Put an input guarded commands"
let text  = parse (Console.ReadLine())
printfn "\n\n"

let mutable map = Map.ofList [("x",0);("y",0) ]
let rec arithm = function
    | N (int) -> int 
    | X key -> (Map.find key map)
    | Plus (a , b) -> arithm a + arithm b
    | Minus (a, b) -> arithm a - arithm b
    | Multiplication (a, b) -> arithm a * arithm b
    | Power (a, b) -> pown (arithm a) (arithm b)
    | UPlus a ->  arithm a 
    | UMinus a -> - arithm a
    | _ -> 1
let rec bool = function
    | True ->  true
    | False ->  false
    | And  (a , b) ->   (bool  a) && (bool  b) 
    | Or  (a, b)  -> (bool  a) || (bool  b)
    | CAnd  (a,  b) ->   (bool  a) && (bool  b) 
    | COr  (a,  b) -> (bool  a) || (bool  b)
    | Not  b -> not (bool  b)
    | Equals  (first , second) -> (arithm  first) = (arithm  second)
    | Unequal  ( a , b ) -> not ((arithm  a) = (arithm  b))
    | Greater ( a , b ) -> ((arithm  a) > (arithm  b))
    | GreaterOrEqual ( a , b ) -> ((arithm  a) >= (arithm  b))
    | Smaller ( a , b ) -> ((arithm  a) < (arithm  b))
    | SmallerOrEqual ( a , b ) -> ((arithm  a) <= (arithm  b))
    | Nested b -> bool  b 


let rec interpret = function
    | Assign ( str, a )                     ->  
        let value = (arithm a)
        map <- (Map.remove str map)
        map <- Map.add str (value) map
        map
    | Skip                                  -> map
    | Command (c1, c2 )                     ->  
        map <- interpret c1  
        map <- interpret c2
        map
    | Do(gc)                                -> interpretGuarded gc
    | _                                     -> map
and interpretGuarded = function
    | SingletonGuarded (b, c) when (bool b) -> 
        map<- interpret c
        map<- interpretGuarded (SingletonGuarded (b, c))
        map
    | Associate(gc1 ,gc2 )                  -> 
        map<- interpretGuarded gc1
        map<- interpretGuarded gc2
        map
    | _                                     -> interpret Skip










printfn "%A" (interpret text)





(*let rec compute =
    try
    let e = parse (Console.ReadLine())
    printfn "ok"
    

    with err -> printfn "ko"

// Start interacting with the user
compute
*)