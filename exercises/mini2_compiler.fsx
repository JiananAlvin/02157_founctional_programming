open System
// Part1: A simple stack machine
type Instruction = 
                 | ADD 
                 | SUB
                 | SIGN
                 | ABS
                 | PUSH of int
type Stack = int List
// interpret the execution of a single instruction
let intpInstr instr stack =
    match (instr,stack) with
    | (ADD,a::b::st) -> (b+a)::st
    | (SUB,a::b::st) -> (b-a)::st
    | (SIGN,a::st)   -> -a::st
    | (ABS,a::st)    -> abs(a)::st
    | (PUSH r,_)     -> r::stack  
    | _              -> stack
// interpret the execution of a program
let exec instrls = 
    let final_stack = List.fold (fun stack instr -> intpInstr instr stack) [] instrls
    match final_stack with
    | top::fst -> top
    | _        -> failwith ("No elemnt in the stack")
                        
// Part2: Expressions: Syntax and semantics
// abstract syntax for expressions
type Exp =
         | X 
         | C of int
         | Add of Exp*Exp
         | Sub of Exp*Exp
         | Minus of Exp
         | Abs of Exp

// semantics (or meaning) of expressions

let rec sem e x =
    match e with
    | X                -> x
    | C c              -> c
    | Add (e1,e2)      -> sem e1 x + sem e2 x
    | Sub (e1,e2)      -> sem e1 x - sem e2 x
    | Minus e          -> - (sem e x)
    | Abs e            -> abs(sem e x)

// Part3: Compilation to stack-machine code
// The complication function is a simple setting for handling the variable X
// instructions list
let rec compile e x =
    match e with
    | X                -> [PUSH x]
    | C c              -> [PUSH c]
    | Add (e1,e2)      -> (compile e1 x)@(compile e2 x)@[ADD]
    | Sub (e1,e2)      -> (compile e1 x)@(compile e2 x)@[SUB]
    | Minus e          -> (compile e x)@[SIGN]
    | Abs e            -> (compile e x)@[ABS]

#r "nuget: FsCheck, 2.16.03"
let check e x = exec (compile e x) = sem e x
let t1 = FsCheck.Check.Quick check

exec (compile (Sub (Minus X, X)) -3)
sem (Sub (Minus X, X)) -3

// Part4: Optimization: Expression reductions
// Check and reduce a base expression
let reduction e =
    match e with
    | Add (C i, C j)    -> C (i+j)
    | Add (e, C 0)      -> e
    | Add (C 0, e)      -> e
    | Sub (C i, C j)    -> C (i-j)
    | Sub (e, C 0)      -> e
    | Sub (C 0, e)      -> Minus e
    | Minus (C i)       -> C (-i)
    | Minus (Minus e)   -> e
    | Abs (C i)         -> C (abs(i))
    | Abs (Minus e)     -> Abs e
    | Abs (Abs e)       -> Abs e
    | _                 -> e

// reduce a huge expression
let rec red e = 
    match e with
    | X                -> X
    | C c              -> C c
    | Add (e1,e2)      -> reduction (Add (reduction (red e1), reduction (red e2)))
    | Sub (e1,e2)      -> reduction (Sub (reduction (red e1), reduction (red e2)))
    | Minus e          -> reduction (Minus (reduction (red e)))
    | Abs e            -> reduction (Abs (reduction (red e)))

let check_red e x = sem e x = sem (red e) x
let t2 = FsCheck.Check.Quick check_red

// check if an expression can be reduced further
let rec reducible =
    function
    | Add (C i, C j) -> true
    | Add (e, C 0) -> true
    | Add (C 0, e) -> true
    | Add (e1, e2) -> reducible e1 || reducible e2
    | Sub (C i, C j) -> true
    | Sub (e, C 0) -> true
    | Sub (C 0, e) -> true 
    | Sub (e1, e2) -> reducible e1 || reducible e2
    | Minus (C i) -> true
    | Minus (Minus e) -> true 
    | Minus (e) -> reducible e
    | Abs (C i) -> true
    | Abs (Minus e) -> true 
    | Abs (Abs e) -> true 
    | Abs (e) -> reducible e
    | C i -> false
    | X -> false

let check_reducible e =  not (reducible(red e))
let t3 = FsCheck.Check.Quick check_reducible
