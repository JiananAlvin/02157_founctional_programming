// Solution to the exam set in 02157 Functional Programming
// 2020 May + 2021 May
//                                                

//Problem 3

type T<'a> = | A of 'a
             | B of 'a * T<'a>
             | C of 'a * T<'a> * T<'a>
             | D of 'a * T<'a> list

//1 type T<int list * string>
let t1 = A([1;2;3], "a")
let t2 = B(([1;1;1], "b"), t1)
let t3 = C(([0;1;0], "c"), t1, t2)
let t4 = A([1;1],"d")
let t5 = D(([4;2;6], "e"), [t1; t2; t3; t4])

//3 ('a -> 'b) -> T<'a> -> T<'b>
let rec mapT f t =
    match t with
    |A v          -> A (f v) 
    |B(v, t1)     -> B(f v, mapT f t1)
    |C(v, t1, t2) -> C(f v, mapT f t1, mapT f t2)

//4
let rec f (xs, s) = (List.sum xs, s) 
mapT f t1
mapT f t2
mapT f t3
mapT f t4

//5 toSet: T<'a> * ('a -> bool) -> Set<'a>
let rec leaves t =
    match t with
    |A v        -> [v]
    |B(v,t1)    -> v::(leaves t1)
    |C(v,t1,t2) -> v::(leaves t1)@(leaves t2)
let toSet(t,p) = Set.ofList (List.filter p (leaves t))
toSet(t3,(fun (xs,s) -> List.sum xs > 0))

//7
let rec mapT1 f t =
    match t with
    |A v          -> A (f v) 
    |B(v, t1)     -> B(f v, mapT f t1)
    |C(v, t1, t2) -> C(f v, mapT f t1, mapT f t2)
    |D(v, ts)     -> D(f v, mapAux f ts)
and mapAux f = function
               | [] -> []
               | t::ts -> (mapT1 f t)::(mapAux f ts)
mapT1 f t5

//7 method2
let rec mapT2 f t =
    match t with
    |A v          -> A (f v) 
    |B(v, t1)     -> B(f v, mapT f t1)
    |C(v, t1, t2) -> C(f v, mapT f t1, mapT f t2)
    |D(v, ts)     -> D(f v, List.map (mapT f) ts)  //  List.map (mapT f) ts: (int list * string->int * string) -> T<int list * string> list -> T<int * string> list
mapT2 f t5

// Problem 2

let rec f x = function
              | [] -> []
              | y::ys -> (x,y)::f x ys
f "a" [1;2;3]

//5  tail-recursive variant based on an accumulating parameter
let rec f1 x acc = function
                  | [] -> List.rev acc
                  | y::ys -> f1 x ((x,y)::acc) ys
f1 "a" [] [1;2;3]

//6 continuation-based tail-recursive variant
let rec f2 x ys c = 
    match ys with
    | [] -> c []
    | y::ys -> f2 x ys (fun v -> c ((x,y)::v))
f2 "a" [1;2;2;3] id

//7 high-order function
let f3 x ys = List.foldBack (fun y rs-> (x,y)::rs) ys []
f3 "a" [1;2;2;3]


// Problem 1              

type Tab<'a, 'b> = ('a * 'b list) list 
let t1: Tab<int, string> = [(1, [ "a"; "b"; "c" ]); (4, [ "b"; "e" ])]

//1 isKey: 'a -> Tab<'a, 'b> -> bool 
let rec isKey x = function
                  | []                      -> false
                  | (key, _)::ts when x=key -> true
                  | t::ts                   -> isKey x ts
isKey 4 t1

//2 insert(x,y,t): 'a * 'b * ('a*'b list) -> ('a*'b list) list
let rec insert(x,y,t) =
    match t with
    | [] -> [(x, [y])]
    | (key, ys)::ts when x=key -> (key, y::ys)::ts
    | t::ts -> t::insert(x,y,ts)
insert(5,"a",t1)

//3 deleteKey: 
let rec deleteKey x t =
    match t with
    | [] -> []
    | (key, _)::ts when x=key -> ts
    | t::ts -> t::(deleteKey x ts)
deleteKey 4 t1

//4 deleteElement y t
let rec deleteElement y t = 
    match t with
    | [] -> []
    | (key,ys1)::ts -> (key, List.filter (fun y' -> y<>y') ys1)::(deleteElement y ts)
deleteElement "e" t1

//5 fromPairs: ('a*'b) list -> Tab<'a,'b>
let fromPairs xs = List.fold (fun t (x,y) -> insert(x,y,t)) [] xs
fromPairs [(2,"c");(1,"a");(2,"b")]

// 2021 May
// Problem 5 
let h f a b =
    let b0 = b
    let rec aux f a b = 
        match (a,b) with
        |(a,_) when a<0  -> Seq.empty
        |(a,b) when b<0  -> aux f (a-1) b0
        |(a,b)           -> Seq.append (aux f a (b-1)) (Seq.ofList [(a,b,f(a,b))])
    aux f a b
Seq.item 4 (h (fun(a,b) -> a + b) 1 2)

let h1 f a b = seq { for i in [0..a] do
                        for j in [0..b] do
                            yield (i,j,f(i,j))}
Seq.item 5 (h1 (fun(a,b) -> a + b) 1 2)
//
seq [1;2;4;6] // Okay