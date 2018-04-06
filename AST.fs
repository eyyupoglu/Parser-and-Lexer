module AST


open System




type a = 
    | N of int
    | X of string
    | Plus of (a * a)
    | Minus of (a * a) 
    | Multiplication of (a *a) 
    | Power of (a * a)
    | Nested of a
    | UPlus of a
    | UMinus of a
    
type b =
    | True
    | False 
    | And of (b * b)
    | Or of (b*b)
    | CAnd of (b * b)
    | COr of (b * b)
    | Not of b
    | Equals of (a * a)
    | Unequal of ( a * a )
    | Greater of (a *a )
    | GreaterOrEqual of (a * a)
    | Smaller of (a *a )
    | SmallerOrEqual of (a * a)
    | Nested of b 


type C = 
    | Assign of (string * a)
    | Skip 
    | Command of (C * C )
    | If of (GC )
    | Do of (GC)
and GC = 
    | SingletonGuarded of (b* C)
    | Associate of (GC * GC)

