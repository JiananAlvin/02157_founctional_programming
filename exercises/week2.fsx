// HR 2.1, 2.2, 2.13,
let HR2_1 x =
    if (x % 2 = 0 || x % 3 = 0) && (x % 5 <> 0) then
        true
    else
        false


let rec HR2_2 (s: string, n: int) =
    match n with
    | 0 -> ""
    | n -> s + HR2_2(s, n - 1)

// HR 2.13
// 
let f (x, y) = x + y + 1
let f1 x y = x + y + 1
let curry f a b = f (a, b)
let incurry f1 (a, b) = f1 a b

// H 4.3
exception NegativeError of string

let rec HR4_3 n =
    match n with
    | 1 -> [ 0 ]
    | _ when n > 0 -> (HR4_3(n - 1)) @ [ (n - 1) * 2 ]
    | _ when n <= 0 -> raise (NegativeError("Input cannot be negative or zero!"))

let split array =
    let rec split_help array (a1, a2) =
        match array with
        | [] -> (List.rev a1, List.rev a2)
        | array_head :: [] -> (List.rev (array_head :: a1), List.rev a2)
        | index1 :: index2 :: array_tail -> split_help array_tail (index1 :: a1, index2 :: a2)

    split_help array ([], [])

let rec get ls (r1, r2) =
    match ls with
    | [] -> (0, 1)
    | head :: tail -> get tail (r1 + head, r2 * head)

exception LengthError of string

let HR4_9 (arr1, arr2) =
    let rec zip (arr1, arr2) arr_zip =
        match (arr1, arr2) with
        | ([], []) -> List.rev arr_zip
        | ([], _)
        | (_, []) -> raise (LengthError("The length is unequal!"))
        | (arr1_head :: arr1_tail, arr2_head :: arr2_tail) ->
            zip (arr1_tail, arr2_tail) ((arr1_head, arr2_head) :: arr_zip)

    zip (arr1, arr2) []

// 4.12
let HR4_12 (p, xs) =
    let rec sum (p, xs) =
        match xs with
        | [] -> 0
        | x :: xs_tail ->
            if p x then
                x + sum (p, xs_tail)
            else
                sum (p, xs_tail)

    sum (p, xs)

// 4.7
(*let rec HR_4_7 (x,xs) =
    match xs with
    | []-> 0
    | y::tails when x=y -> 1+HR_4_7(x,tails)
    | y::tails -> HR_4_7(x,tails)*)
