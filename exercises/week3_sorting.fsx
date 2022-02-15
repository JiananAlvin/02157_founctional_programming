//week3 sorting
module SortAndPBT

open FsCheck

let rec merge lst1 lst2 =
    match (lst1, lst2) with
    | (l1head :: _, l2head :: l2tails) when l1head >= l2head -> l2head :: merge lst1 l2tails
    | (l1head :: l1tails, l2head :: _) when l1head < l2head -> l1head :: merge l1tails lst2
    | ([], []) -> []
    | ([], _) -> lst2
    | (_, []) -> lst1


let split array =
    let rec split_help array (a1, a2) =
        match array with
        | [] -> (List.rev a1, List.rev a2)
        | array_head :: [] -> (List.rev (array_head :: a1), List.rev a2)
        | index1 :: index2 :: array_tail -> split_help array_tail (index1 :: a1, index2 :: a2)

    split_help array ([], [])


let sort array_ori =
    let rec sort_help array =
        let (array1, array2) = split array

        match array with
        | head :: [] -> [ head ]
        | [] -> []
        | _ :: _ -> merge (sort_help array1) (sort_help array2)

    sort_help array_ori

let rec ordered xs =
    match xs with
    | head1 :: head2 :: tails ->
        if head1 <= head2 then
            true && ordered (head2 :: tails)
        else
            false
    | head1 :: [] -> true
    | [] -> true

(*printfn "%A" (merge [1;4;9;12] [2;3;4;5;10;13])
printfn "%A" (split [1; 2; 3; 4; 4; 5; 9; 10; 12; 13])*)
(*printfn "%A" (sort [2;6;7;8;3;5;1;2;4;23;6;4;55])*)

printfn
    "%b"
    (ordered (
        sort [ 2
               6
               7
               8
               3
               5
               1
               2
               4
               23
               6
               4
               55 ]
    ))



let orderedSort (xs: int list) = ordered (sort xs)

let increment (x, cnt) =
    let rec inc (x, cnt) =
        match cnt with
        | (key, value) :: tails ->
            if key = x then
                value + 1
            else
                inc (x, tails)
        | [] -> 1

    inc (x, cnt)

let toCounting xs =
    let rec count xs temp =
        match xs with
        | head :: tails when (increment (head, temp) = 1) -> count tails ((head, 1) :: temp)
        | head :: tails ->
            count
                tails
                (List.map
                    (fun (key, value) ->
                        if key = head then
                            (key, value + 1)
                        else
                            (key, value))
                    temp)
        | [] -> temp

    count xs []

let p = [ 1; 2; 1; 1; 1; 6 ]
toCounting p

