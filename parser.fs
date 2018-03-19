// Implementation file for parser generated by fsyacc
module parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "parser.fsp"

open AST

# 10 "parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | SMALLEROREQUAL
  | ARROW
  | BOX
  | SKIP
  | SEMI
  | IF
  | FI
  | DO
  | OD
  | TRUE
  | FALSE
  | AND
  | OR
  | CAND
  | COR
  | NOT
  | EQUALS
  | UNEQUAL
  | GREATER
  | GREATEROREQUAL
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | LPAR
  | RPAR
  | EOF
  | ASSIGN
  | SMALLER
  | NUM of (int)
  | NAME of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_SMALLEROREQUAL
    | TOKEN_ARROW
    | TOKEN_BOX
    | TOKEN_SKIP
    | TOKEN_SEMI
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_CAND
    | TOKEN_COR
    | TOKEN_NOT
    | TOKEN_EQUALS
    | TOKEN_UNEQUAL
    | TOKEN_GREATER
    | TOKEN_GREATEROREQUAL
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_EOF
    | TOKEN_ASSIGN
    | TOKEN_SMALLER
    | TOKEN_NUM
    | TOKEN_NAME
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_b
    | NONTERM_a
    | NONTERM_gc
    | NONTERM_c

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | SMALLEROREQUAL  -> 0 
  | ARROW  -> 1 
  | BOX  -> 2 
  | SKIP  -> 3 
  | SEMI  -> 4 
  | IF  -> 5 
  | FI  -> 6 
  | DO  -> 7 
  | OD  -> 8 
  | TRUE  -> 9 
  | FALSE  -> 10 
  | AND  -> 11 
  | OR  -> 12 
  | CAND  -> 13 
  | COR  -> 14 
  | NOT  -> 15 
  | EQUALS  -> 16 
  | UNEQUAL  -> 17 
  | GREATER  -> 18 
  | GREATEROREQUAL  -> 19 
  | TIMES  -> 20 
  | DIV  -> 21 
  | PLUS  -> 22 
  | MINUS  -> 23 
  | POW  -> 24 
  | LPAR  -> 25 
  | RPAR  -> 26 
  | EOF  -> 27 
  | ASSIGN  -> 28 
  | SMALLER  -> 29 
  | NUM _ -> 30 
  | NAME _ -> 31 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_SMALLEROREQUAL 
  | 1 -> TOKEN_ARROW 
  | 2 -> TOKEN_BOX 
  | 3 -> TOKEN_SKIP 
  | 4 -> TOKEN_SEMI 
  | 5 -> TOKEN_IF 
  | 6 -> TOKEN_FI 
  | 7 -> TOKEN_DO 
  | 8 -> TOKEN_OD 
  | 9 -> TOKEN_TRUE 
  | 10 -> TOKEN_FALSE 
  | 11 -> TOKEN_AND 
  | 12 -> TOKEN_OR 
  | 13 -> TOKEN_CAND 
  | 14 -> TOKEN_COR 
  | 15 -> TOKEN_NOT 
  | 16 -> TOKEN_EQUALS 
  | 17 -> TOKEN_UNEQUAL 
  | 18 -> TOKEN_GREATER 
  | 19 -> TOKEN_GREATEROREQUAL 
  | 20 -> TOKEN_TIMES 
  | 21 -> TOKEN_DIV 
  | 22 -> TOKEN_PLUS 
  | 23 -> TOKEN_MINUS 
  | 24 -> TOKEN_POW 
  | 25 -> TOKEN_LPAR 
  | 26 -> TOKEN_RPAR 
  | 27 -> TOKEN_EOF 
  | 28 -> TOKEN_ASSIGN 
  | 29 -> TOKEN_SMALLER 
  | 30 -> TOKEN_NUM 
  | 31 -> TOKEN_NAME 
  | 34 -> TOKEN_end_of_input
  | 32 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_b 
    | 3 -> NONTERM_b 
    | 4 -> NONTERM_b 
    | 5 -> NONTERM_b 
    | 6 -> NONTERM_b 
    | 7 -> NONTERM_b 
    | 8 -> NONTERM_b 
    | 9 -> NONTERM_b 
    | 10 -> NONTERM_b 
    | 11 -> NONTERM_b 
    | 12 -> NONTERM_b 
    | 13 -> NONTERM_b 
    | 14 -> NONTERM_b 
    | 15 -> NONTERM_b 
    | 16 -> NONTERM_a 
    | 17 -> NONTERM_a 
    | 18 -> NONTERM_a 
    | 19 -> NONTERM_a 
    | 20 -> NONTERM_a 
    | 21 -> NONTERM_a 
    | 22 -> NONTERM_a 
    | 23 -> NONTERM_a 
    | 24 -> NONTERM_a 
    | 25 -> NONTERM_gc 
    | 26 -> NONTERM_gc 
    | 27 -> NONTERM_c 
    | 28 -> NONTERM_c 
    | 29 -> NONTERM_c 
    | 30 -> NONTERM_c 
    | 31 -> NONTERM_c 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 34 
let _fsyacc_tagOfErrorTerminal = 32

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | SMALLEROREQUAL  -> "SMALLEROREQUAL" 
  | ARROW  -> "ARROW" 
  | BOX  -> "BOX" 
  | SKIP  -> "SKIP" 
  | SEMI  -> "SEMI" 
  | IF  -> "IF" 
  | FI  -> "FI" 
  | DO  -> "DO" 
  | OD  -> "OD" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | AND  -> "AND" 
  | OR  -> "OR" 
  | CAND  -> "CAND" 
  | COR  -> "COR" 
  | NOT  -> "NOT" 
  | EQUALS  -> "EQUALS" 
  | UNEQUAL  -> "UNEQUAL" 
  | GREATER  -> "GREATER" 
  | GREATEROREQUAL  -> "GREATEROREQUAL" 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | POW  -> "POW" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | EOF  -> "EOF" 
  | ASSIGN  -> "ASSIGN" 
  | SMALLER  -> "SMALLER" 
  | NUM _ -> "NUM" 
  | NAME _ -> "NAME" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | SMALLEROREQUAL  -> (null : System.Object) 
  | ARROW  -> (null : System.Object) 
  | BOX  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | SEMI  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | FI  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | OD  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | CAND  -> (null : System.Object) 
  | COR  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | EQUALS  -> (null : System.Object) 
  | UNEQUAL  -> (null : System.Object) 
  | GREATER  -> (null : System.Object) 
  | GREATEROREQUAL  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | POW  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | ASSIGN  -> (null : System.Object) 
  | SMALLER  -> (null : System.Object) 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 9us; 65535us; 13us; 6us; 14us; 7us; 15us; 8us; 16us; 9us; 17us; 10us; 32us; 11us; 57us; 12us; 63us; 12us; 65us; 12us; 23us; 65535us; 13us; 18us; 14us; 18us; 15us; 18us; 16us; 18us; 17us; 18us; 20us; 21us; 22us; 23us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 32us; 19us; 44us; 36us; 45us; 37us; 46us; 38us; 47us; 39us; 48us; 40us; 49us; 41us; 50us; 42us; 57us; 18us; 59us; 43us; 63us; 18us; 65us; 18us; 3us; 65535us; 57us; 54us; 63us; 55us; 65us; 56us; 3us; 65535us; 0us; 2us; 52us; 53us; 62us; 61us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 13us; 37us; 41us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 29us; 1us; 1us; 1us; 2us; 1us; 3us; 5us; 4us; 4us; 5us; 6us; 7us; 5us; 4us; 5us; 5us; 6us; 7us; 5us; 4us; 5us; 6us; 6us; 7us; 5us; 4us; 5us; 6us; 7us; 7us; 5us; 4us; 5us; 6us; 7us; 8us; 5us; 4us; 5us; 6us; 7us; 15us; 5us; 4us; 5us; 6us; 7us; 25us; 1us; 4us; 1us; 5us; 1us; 6us; 1us; 7us; 1us; 8us; 10us; 9us; 10us; 11us; 12us; 13us; 14us; 18us; 19us; 20us; 21us; 11us; 9us; 10us; 11us; 12us; 13us; 14us; 18us; 19us; 20us; 21us; 24us; 1us; 9us; 5us; 9us; 18us; 19us; 20us; 21us; 1us; 10us; 5us; 10us; 18us; 19us; 20us; 21us; 1us; 11us; 5us; 11us; 18us; 19us; 20us; 21us; 1us; 12us; 5us; 12us; 18us; 19us; 20us; 21us; 1us; 13us; 5us; 13us; 18us; 19us; 20us; 21us; 1us; 14us; 5us; 14us; 18us; 19us; 20us; 21us; 2us; 15us; 24us; 1us; 15us; 1us; 16us; 1us; 17us; 5us; 18us; 18us; 19us; 20us; 21us; 5us; 18us; 19us; 19us; 20us; 21us; 5us; 18us; 19us; 20us; 20us; 21us; 5us; 18us; 19us; 20us; 21us; 21us; 5us; 18us; 19us; 20us; 21us; 22us; 5us; 18us; 19us; 20us; 21us; 23us; 5us; 18us; 19us; 20us; 21us; 24us; 5us; 18us; 19us; 20us; 21us; 27us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 21us; 1us; 22us; 1us; 23us; 1us; 24us; 1us; 24us; 1us; 25us; 2us; 25us; 29us; 2us; 26us; 26us; 2us; 26us; 30us; 2us; 26us; 31us; 1us; 26us; 1us; 27us; 1us; 27us; 1us; 28us; 2us; 29us; 29us; 1us; 29us; 1us; 30us; 1us; 30us; 1us; 31us; 1us; 31us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 9us; 11us; 13us; 19us; 25us; 31us; 37us; 43us; 49us; 55us; 57us; 59us; 61us; 63us; 65us; 76us; 88us; 90us; 96us; 98us; 104us; 106us; 112us; 114us; 120us; 122us; 128us; 130us; 136us; 139us; 141us; 143us; 145us; 151us; 157us; 163us; 169us; 175us; 181us; 187us; 193us; 195us; 197us; 199us; 201us; 203us; 205us; 207us; 209us; 211us; 214us; 217us; 220us; 223us; 225us; 227us; 229us; 231us; 234us; 236us; 238us; 240us; 242us; |]
let _fsyacc_action_rows = 67
let _fsyacc_actionTableElements = [|4us; 32768us; 3us; 60us; 5us; 63us; 7us; 65us; 31us; 58us; 0us; 49152us; 2us; 32768us; 4us; 62us; 27us; 3us; 0us; 16385us; 0us; 16386us; 0us; 16387us; 4us; 16388us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 4us; 16389us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 4us; 16390us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 4us; 16391us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 4us; 16392us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 5us; 32768us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 26us; 33us; 5us; 32768us; 1us; 52us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 8us; 32768us; 9us; 4us; 10us; 5us; 15us; 17us; 22us; 48us; 23us; 49us; 25us; 32us; 30us; 34us; 31us; 35us; 8us; 32768us; 9us; 4us; 10us; 5us; 15us; 17us; 22us; 48us; 23us; 49us; 25us; 32us; 30us; 34us; 31us; 35us; 8us; 32768us; 9us; 4us; 10us; 5us; 15us; 17us; 22us; 48us; 23us; 49us; 25us; 32us; 30us; 34us; 31us; 35us; 8us; 32768us; 9us; 4us; 10us; 5us; 15us; 17us; 22us; 48us; 23us; 49us; 25us; 32us; 30us; 34us; 31us; 35us; 8us; 32768us; 9us; 4us; 10us; 5us; 15us; 17us; 22us; 48us; 23us; 49us; 25us; 32us; 30us; 34us; 31us; 35us; 10us; 32768us; 0us; 30us; 16us; 20us; 17us; 22us; 18us; 24us; 19us; 26us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 29us; 28us; 11us; 32768us; 0us; 30us; 16us; 20us; 17us; 22us; 18us; 24us; 19us; 26us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 26us; 51us; 29us; 28us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 4us; 16393us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 4us; 16394us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 4us; 16395us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 4us; 16396us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 4us; 16397us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 4us; 16398us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 8us; 32768us; 9us; 4us; 10us; 5us; 15us; 17us; 22us; 48us; 23us; 49us; 25us; 32us; 30us; 34us; 31us; 35us; 0us; 16399us; 0us; 16400us; 0us; 16401us; 2us; 16402us; 20us; 46us; 24us; 47us; 2us; 16403us; 20us; 46us; 24us; 47us; 1us; 16404us; 24us; 47us; 1us; 16405us; 24us; 47us; 2us; 16406us; 20us; 46us; 24us; 47us; 2us; 16407us; 20us; 46us; 24us; 47us; 5us; 32768us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 26us; 51us; 4us; 16411us; 20us; 46us; 22us; 44us; 23us; 45us; 24us; 47us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 0us; 16408us; 4us; 32768us; 3us; 60us; 5us; 63us; 7us; 65us; 31us; 58us; 1us; 16409us; 4us; 62us; 1us; 16410us; 2us; 57us; 2us; 32768us; 2us; 57us; 6us; 64us; 2us; 32768us; 2us; 57us; 8us; 66us; 8us; 32768us; 9us; 4us; 10us; 5us; 15us; 17us; 22us; 48us; 23us; 49us; 25us; 32us; 30us; 34us; 31us; 35us; 1us; 32768us; 28us; 59us; 5us; 32768us; 22us; 48us; 23us; 49us; 25us; 50us; 30us; 34us; 31us; 35us; 0us; 16412us; 1us; 16413us; 4us; 62us; 4us; 32768us; 3us; 60us; 5us; 63us; 7us; 65us; 31us; 58us; 8us; 32768us; 9us; 4us; 10us; 5us; 15us; 17us; 22us; 48us; 23us; 49us; 25us; 32us; 30us; 34us; 31us; 35us; 0us; 16414us; 8us; 32768us; 9us; 4us; 10us; 5us; 15us; 17us; 22us; 48us; 23us; 49us; 25us; 32us; 30us; 34us; 31us; 35us; 0us; 16415us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 9us; 10us; 11us; 12us; 17us; 22us; 27us; 32us; 37us; 43us; 49us; 58us; 67us; 76us; 85us; 94us; 105us; 117us; 123us; 128us; 134us; 139us; 145us; 150us; 156us; 161us; 167us; 172us; 178us; 183us; 192us; 193us; 194us; 195us; 198us; 201us; 203us; 205us; 208us; 211us; 217us; 222us; 228us; 234us; 240us; 246us; 252us; 258us; 264us; 265us; 270us; 272us; 274us; 277us; 280us; 289us; 291us; 297us; 298us; 300us; 305us; 314us; 315us; 324us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 1us; 3us; 3us; 3us; 3us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; 1us; 3us; 3us; 3us; 3us; 2us; 2us; 3us; 3us; 3us; 3us; 1us; 3us; 3us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; 5us; 5us; 5us; 5us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16386us; 16387us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16399us; 16400us; 16401us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16408us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16412us; 65535us; 65535us; 65535us; 16414us; 65535us; 16415us; |]
let _fsyacc_reductions ()  =    [| 
# 287 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : C)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 296 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : C)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "parser.fsp"
                                                _1 
                   )
# 25 "parser.fsp"
                 : C));
# 307 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "parser.fsp"
                                   True 
                   )
# 28 "parser.fsp"
                 : b));
# 317 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "parser.fsp"
                                    False 
                   )
# 29 "parser.fsp"
                 : b));
# 327 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "parser.fsp"
                                      And( _1, _3) 
                   )
# 30 "parser.fsp"
                 : b));
# 339 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "parser.fsp"
                                     Or( _1, _3) 
                   )
# 31 "parser.fsp"
                 : b));
# 351 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "parser.fsp"
                                      CAnd(_1, _3)
                   )
# 32 "parser.fsp"
                 : b));
# 363 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "parser.fsp"
                                      COr( _1, _3)
                   )
# 33 "parser.fsp"
                 : b));
# 375 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "parser.fsp"
                                    Not(_2)
                   )
# 34 "parser.fsp"
                 : b));
# 386 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "parser.fsp"
                                        Equals(_1 , _3)
                   )
# 35 "parser.fsp"
                 : b));
# 398 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "parser.fsp"
                                         Unequal(_1 , _3)
                   )
# 36 "parser.fsp"
                 : b));
# 410 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "parser.fsp"
                                         Greater(_1 , _3)
                   )
# 37 "parser.fsp"
                 : b));
# 422 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "parser.fsp"
                                              GreaterOrEqual(_1 , _3)
                   )
# 38 "parser.fsp"
                 : b));
# 434 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "parser.fsp"
                                         Smaller(_1 , _3)
                   )
# 39 "parser.fsp"
                 : b));
# 446 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "parser.fsp"
                                              SmallerOrEqual(_1 , _3)
                   )
# 40 "parser.fsp"
                 : b));
# 458 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "parser.fsp"
                                         Nested(_2)
                   )
# 41 "parser.fsp"
                 : b));
# 469 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "parser.fsp"
                                   N(_1) 
                   )
# 44 "parser.fsp"
                 : a));
# 480 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "parser.fsp"
                                   X(_1) 
                   )
# 45 "parser.fsp"
                 : a));
# 491 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "parser.fsp"
                                      Plus(_1,_3) 
                   )
# 46 "parser.fsp"
                 : a));
# 503 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "parser.fsp"
                                       Minus(_1,_3) 
                   )
# 47 "parser.fsp"
                 : a));
# 515 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "parser.fsp"
                                       Multiplication(_1,_3) 
                   )
# 48 "parser.fsp"
                 : a));
# 527 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "parser.fsp"
                                         Power(_1,_3) 
                   )
# 49 "parser.fsp"
                 : a));
# 539 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "parser.fsp"
                                            UPlus(_2) 
                   )
# 50 "parser.fsp"
                 : a));
# 550 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "parser.fsp"
                                            UMinus(_2) 
                   )
# 51 "parser.fsp"
                 : a));
# 561 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "parser.fsp"
                                            _2 
                   )
# 52 "parser.fsp"
                 : a));
# 572 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : C)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "parser.fsp"
                                      SingletonGuarded(_1, _3) 
                   )
# 56 "parser.fsp"
                 : GC));
# 584 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : GC)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : GC)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "parser.fsp"
                                       Associate(_1, _3) 
                   )
# 57 "parser.fsp"
                 : GC));
# 596 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "parser.fsp"
                                             Assign(_1, _3) 
                   )
# 60 "parser.fsp"
                 : C));
# 608 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "parser.fsp"
                                   Skip 
                   )
# 61 "parser.fsp"
                 : C));
# 618 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : C)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : C)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "parser.fsp"
                                      Command(_1, _3) 
                   )
# 62 "parser.fsp"
                 : C));
# 630 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : GC)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "parser.fsp"
                                      If(_2) 
                   )
# 63 "parser.fsp"
                 : C));
# 641 "parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : GC)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "parser.fsp"
                                      Do(_2) 
                   )
# 64 "parser.fsp"
                 : C));
|]
# 653 "parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 35;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : C =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))