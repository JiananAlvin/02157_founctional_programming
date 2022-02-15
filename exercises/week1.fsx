module Week1
(*
Homework:
1.HR 1.4, HR 1.6, HR 2.8, HR 4.7
2.Mini-project on polynomials: Part 1 See Material folder on DTU Learn*)
// HR1.4
let rec HR1_4 x =
    match x with
    | 0 -> 0
    | _ -> x + HR1_4(x - 1)
//==================================================================================
let rec HR1_6 m n : int =
    match m, n with
    | _, 0 -> m + 0
    | _, _ -> m + n + (HR1_6 m (n - 1))
//==================================================================================

let rec bin (m, n) =
    match (m, n) with
    | (_, 0) -> 1
    | (m, n) when m = n -> 1
    | (m, n) -> (bin (m - 1, n - 1)) + (bin (m - 1, n))

(*let printLine lineNum maxLevel =
    let seq1 = seq {for i in 0 .. lineNum -> bin lineNum i}
    let numForBlank = seq {for i in 0 .. (maxLevel-lineNum)/2-> i}

    for i in numForBlank do
        printf "\t "
    for i in seq1 do
        printf "\t %d" i
    printf "\n"


let rec HR2_8 level maxLevel:unit =
    match level with
    | 0 -> printLine 0 maxLevel
    | _ -> HR2_8 (level-1) maxLevel ; printLine level maxLevel*)

//==================================================================================

let rec HR_4_7 (x, xs) =
    match xs with
    | [] -> 0
    | y :: tails when x = y -> 1 + HR_4_7(x, tails)
    | y :: tails -> HR_4_7(x, tails)
