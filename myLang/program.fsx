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

let text  = parse (Console.ReadLine())

printfn "\n\n"
//helping function for creating an edge
let rec edge start endd act = 
    String.Format("q{0} ->  q{1} [label = \"{2}\"];\n", start, endd, act)


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


let mutable nat =  seq []
let mutable counter = seq []

//This function finds the last element of a list
let rec last tmp = function
    | [] -> tmp
    | a::b -> last a b 

let rec compile (s: int)= function
    | Assign ( str, num )   ->  
        nat <- Seq.append (nat) (seq [edge (string s) (string (s+1)) ( str + ":=" + (arithm num))])
        counter <- Seq.append (counter) (seq [s])
        edge (string s) (string (s+1)) ( str + ":=" + (arithm num)) 
    | Skip                  ->
        nat <- Seq.append (nat) (seq [edge (string s) (string (s+1)) "skip"])
        counter <- Seq.append (counter) (seq [s])
        edge (string s) (string (s+1)) "skip"
    | Command (c1, c2 )     ->  
        compile s c1 + compile (s+1) c2 
    | If (gc)               ->  compile2 (s)  gc 
    | Do (gc)               ->  compile2 (s)  gc 
and compile2 s  = function
    | SingletonGuarded (b, c) ->
        nat <- Seq.append (nat) (seq [edge (string s) (string (s+1)) (bool b)])
        counter <- Seq.append (counter) (seq [s])
        let lastOne = string (last (Seq.toList nat))
        edge (string s) (string (s+1)) (bool b) + 
        compile (s+1)  c 
        
    | Associate (gc1 ,gc2 )   -> compile2 (s+1)  gc1  + compile2 (s+2)  gc2 

//helping function
let printSeq seq1 = Seq.iter (printf "%A ") seq1; printfn ""


printfn "%A" (compile 0  text)
nat |> Seq.iter (fun x -> printf "%s " x)
printSeq counter




(*let rec compute =
    try
    let e = parse (Console.ReadLine())
    printfn "ok"
    

    with err -> printfn "ko"

// Start interacting with the user
compute
*)