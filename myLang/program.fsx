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
//helping
let rec edge start endd act = 
    String.Format("q{0} ->  q{1} [label = \"{2}\"];\n", start, endd, act)
//helping function
let printSeq seq1 = Seq.iter (printf "%A ") seq1; printfn ""
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
let mutable states =   []
//This function finds the last element of a list
let rec last tmp = function
    | [] -> tmp
    | a::b -> last a b 
let rec findMax (n: int) = function
    | [] -> n
    | a::b when a > n -> findMax a b
    | a::b -> findMax n b
let newStateNumber liste = (findMax 0 liste) + 1
let addStateToStateList stateList stateNumber = List.append stateList [stateNumber]

let rec DONE guardedC =
    match guardedC with
    | SingletonGuarded (b, c) -> Not b
    | Associate (gc1, gc2) -> And (DONE gc1, DONE gc2)

let rec edges (s: int) (e: int) = function
    | Assign ( str, num )   ->  
        let newNode = newStateNumber states//fresh q
        states <- addStateToStateList states newNode//returns new list of states
        edge (string s) (string e) ( str + ":=" + (arithm num)) 
    | Skip                  ->
        let newNode = newStateNumber states//fresh q
        states <- addStateToStateList states newNode//returns new list of states
        edge (string s) (string e) "skip"
    | Command (c1, c2 )     ->  
        let newNode = newStateNumber states//fresh q 
        edges s newNode c1 + edges newNode e c2
    | Do (gc)               ->  compileDo s e gc
    | If (gc)               ->  compileIf s e gc
and compileDo (s: int) (e: int) = function
    | SingletonGuarded (b, c) ->
        let newNode = newStateNumber states//fresh q1
        states <- addStateToStateList states newNode//returns new list of states

        let newNode2 = newStateNumber states//fresh q2 for Not(bool)
        states <- addStateToStateList states newNode2//returns new list of states

        edge (string s) (string (newNode)) (bool b) +
        edges newNode s c + 
        edge (string s) (string newNode2) ("!"+bool b) 
    | Associate (gc1 ,gc2 )   -> compileDo s e gc1  + compileDo s e gc2
and compileIf (s: int) (e: int) = function
   | SingletonGuarded (b, c) ->       
        let newNode = newStateNumber states//fresh q
        states <- addStateToStateList states newNode//returns new list of states
        edge (string s) (string (newNode)) (bool b) +
        edges newNode e c 
    | Associate (gc1 ,gc2 )   -> compileIf s e gc1  + compileIf s e gc2











printfn "%A" (edges 0 999 text)





(*let rec compute =
    try
    let e = parse (Console.ReadLine())
    printfn "ok"
    

    with err -> printfn "ko"

// Start interacting with the user
compute
*)