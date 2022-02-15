//week10
// 2015 Dec P 2.2.2
(***
let rec g1 p xl c =
    match xl with
    | x :: xs ->
        if p x then
            g1 p xs (fun v -> c (x :: v))
        else
            g1 p xs (fun v -> c v)
    | _ -> c []
***)

// correct solution
let rec g1 p xl c =
    match xl with
    | x :: xs when p x -> g1 p xs (fun v -> c (x :: v))
    | _ -> c []

g1 (fun x -> x > 3) [ 4; 2; 8; 1; 2; 0; 5 ] id

let rec g0 p =
    function
    | x :: xs when p x -> x :: g0 p xs
    | _ -> []

g0 (fun x -> x > 3) [ 4; 2; 8; 1; 2; 0; 5 ]

// 2011 P3
type 'a tree = | Lf
               | Br of 'a * 'a tree * 'a tree;;

let rec f(n,t) =
    match t with
    | Lf -> Lf
    | Br(a, t1, t2) -> if n>0 then Br(a, f(n-1, t1), f(n-1, t2))
                       else Lf;;
let t = Br (1, Br (4, Br (3, Br (2, Lf, Lf), Br (5, Lf, Lf)), Br (3, Lf, Lf)), Br (7, Lf, Br (4, Br (6, Lf, Lf), Br (9, Lf, Lf))))
f(3,t)

let rec g p = 
    function
    | Br (a, t1, t2) when p a -> Br (a, g p t1, g p t2)
    | _ -> Lf;;

g (fun x -> x < 4) t

let rec h k = 
    function
    | Lf -> Lf
    | Br(a, t1, t2) -> Br(k a, h k t1, h k t2);;
h (fun x -> x*2) t
