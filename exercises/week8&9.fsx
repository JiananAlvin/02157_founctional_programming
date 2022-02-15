// week 8&9
// 2019 summer P1 Q6 high-order function
let f x yl = List.map (fun y -> (x, y)) yl
f "a" [ 1; 2; 3 ]
//
type exp =
    | C of int
    | BinOp of exp * string * exp
    | Id of string
    | Def of string * exp * exp

(*
[1;2;3]
[(C 1,"w",C 7);(C 0,"n",C 7);(C 6,"o",C 7)]
*)

// 2015 Dec P1 1.2
type Appliance = string
type Usage = Appliance * int
let ad1 = ("washing machine", 1)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ ad1; ad2; ad3; ad1; ad2 ]

let rec durationOf a ats =
    match (a, ats) with
    | ((_, t), []) -> 0
    | ((_, t), atsh :: atst) when a = atsh -> t + (durationOf a atst)
    | ((_, t), atsh :: atst) -> durationOf a atst

durationOf ad2 ats

// 1.1
let rec inv ats =
    match ats with
    | [] -> true
    | (_, t) :: atst when t > 0 -> inv atst
    | _ -> false

inv ats

// 1.3
let rec wellFormed ats =
    List.forall (fun a -> (durationOf a ats) <= 24) ats
    && inv ats

wellFormed ats

// week 8
// 2011 P2
// 2. toString: exp -> string
let rec toString =
    function
    | C n -> string n
    | BinOp (e1, s, e2) -> "(" + (toString e1) + s + (toString e2) + ")"

toString (BinOp(C 3, "+", BinOp(C 5, "*", C 2)))
toString (BinOp(BinOp(C 2, "*", C 6), "+", BinOp(BinOp(C 9, "+", C 4), "*", C 2)))

// 3. Extracting the set of operators from an expression
let rec ops =
    function
    | C n -> []
    | BinOp (e1, s, e2) -> [ s ] @ (ops e1) @ (ops e2)

ops (BinOp(BinOp(C 2, "*", C 6), "+", BinOp(BinOp(C 9, "+", C 4), "*", C 2)))

// 4. isDef: exp -> bool
let rec f id e =
    match e with
    | BinOp (e1, s, e2) -> isDef id e1 && isDef id e2
    | Def (id, e1, e2) -> isDef id e1 && isDef id e2
    | _ -> isDef id e

and isDef id e =
    match e with
    | C n -> true
    | Id x when id = x -> true
    | _ -> f id e

isDef "" (Def("x", C 5, BinOp(Id "y", "+", Id "x")))
isDef "" (Def("y", C 5, BinOp(Id "y", "+", Id "x")))
isDef "" (Def("y", C 5, BinOp(Id "y", "+", Id "y")))

// 2015 Dec P3
type Name = string
type Flow = int // can be assumed positive in below questions

type River = R of Name * Flow * Tributaries
and Tributaries = River list
// 1. Declare F# values
let riv =
    R(
        "R",
        10,
        [ R("R1", 5, [])
          R("R2", 15, [ R("R4", 2, []) ])
          R("R3", 8, []) ]
    )

let riv3 = R("R3", 8, [])
let riv2 = R("R2", 15, [ R("R4", 2, []) ])
// 2. cotains: Name->River->bool
let rec contains n r =
    match r with
    | R (n0, _, _) when n = n0 -> true
    | R (_, _, t) -> false || (trib n t)

and trib n r =
    match r with
    | [] -> false
    | rh :: rt -> (contains n rh) || (trib n rt)

contains "R" riv3

// 3. allNames: River -> Name list
let rec allNames r =
    match r with
    | R (n, _, t) -> n :: (tribNames t)

and tribNames r =
    match r with
    | [] -> []
    | rh :: rt -> (allNames rh) @ (tribNames rt)

allNames riv2


// 4. totalFlow: River -> Flow
let rec totalFlow r =
    match r with
    | R (_, f, t) -> f + (tribFlow t)

and tribFlow r =
    match r with
    | [] -> 0
    | rh :: rt -> (totalFlow rh) + (tribFlow rt)

totalFlow riv

// 5. mainSource: River -> (Name*Flow)
let mainSource r =
    let ms = ("n", 0)

    let rec source r ms =
        match (r, ms) with
        | (R (n, f, t), (_, mf)) when f > mf -> tribSource t (n, f)
        | (R (_, _, t), ms) -> tribSource t ms

    and tribSource r ms =
        match (r, ms) with
        | ([], ms) -> ms
        | (rh :: rt, ms) -> tribSource rt (source rh ms)

    source r ms

mainSource riv

// 6. tryInsert: Name -> River -> River -> River option
let tryInsert n t r =
    if not (contains n r) then
        None
    else
        let rec insert n t r =
            match r with
            | R (n0, f, t0) when n = n0 -> R(n0, f, t :: t0)
            | R (n0, f, t0) -> R(n0, f, (tribInsert n t t0))

        and tribInsert n t r =
            match r with
            | [] -> []
            | th :: tt -> (insert n t th) :: (tribInsert n t tt)

        Some(insert n t r)

tryInsert "R" (R("R5", 100, [])) riv
