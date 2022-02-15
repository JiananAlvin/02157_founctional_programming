// Summer Exam 2014 Problems 2 and 3               Michael 29-11-2021

// Problem 2

// Q1
let rec ordered = function | n1::n2::ns -> n1<= n2 && ordered(n2::ns)
                           | _          -> true;;   

// Q2
let smallerThanAll m ns = List.forall (fun n -> m<n) ns;;

let rec smallerThanAll1 x = function
                       | [] -> true
                       | x'::xs -> x'>=x && smallerThanAll1 x xs

smallerThanAll1 2 [2; 3; 4; 5; 6; 7]


// Q3
let rec insertBefore p e = function | [] -> [e] 
                                    | x::xs when p x -> e::x::xs
                                    | x::xs          -> x::insertBefore p e xs;;

// Q4
type Sex = | M              // male
           | F              // female
let sexToString = function | M -> "Male" | F -> "Female";;


// Q5
let rec replicate n str = match n with 
                          | 0          -> ""
                          | _ when n>0 -> str + replicate (n-1) str                         
                          | _          -> failwith "A string cannot be repeated less than 0 times" 

let rec replicate1 n str =
    if n = 0 then
        ""
    else if n < 0 then
        failwith ("A string cannot be repeated less than 0 times")
    else
        str + replicate1 (n - 1) str

replicate1 4 "a"



// Problem 3

type Name = string
type YearOfBirth = int

type FamilyTree = P of Name * Sex * YearOfBirth * Children 
and Children = FamilyTree list


// Q1
let rec isWellFormedTree(P(_,_,y,cs)) = let ys = List.map (fun (P(_,_,cy,_)) -> cy) cs
                                        smallerThanAll y ys && ordered ys && isWellFormedChildren cs

and isWellFormedChildren ds = List.forall isWellFormedTree ds;;

// Q2
let makePerson n s y = P(n,s,y,[])

// Q3
let rec insertChildOf p (P(_,_,yc,_) as ct) (P(n,s,y,cs) as ft) = 
    if yc<=y then None
    else if p=n then Some(P(n,s,y,insertBefore (fun (P(_,_,y,_)) -> yc<=y) ct cs))
         else match insertChildInList p ct cs with
              | Some cs' -> Some(P(n,s,y,cs'))
              | None     -> None

and insertChildInList p ct = 
     function
     | []     -> None  
     | c1::cs -> match insertChildOf p ct c1 with                                                                      
                     | Some ft -> Some(ft::cs)
                     | None    -> match insertChildInList p ct cs with 
                                  | Some cs' -> Some(c1::cs')
                                  | None     -> None;;

// Q4
// FamilyTree
let rec find p (P(n,s,y,cs)) = 
    if p=n then Some(s,y,List.map (fun (P(nc,_,_,_)) -> nc) cs)
    else findInList p cs 
// Children (list)
and findInList p = function | []    -> None
                            | c::cs -> match find p c with 
                                       | None -> findInList p cs
                                       | res  -> res;;

// Q5 
let rec personToS k i (P(n,s,y,cs)) =    replicate (k*i) " " + n + " " + sexToString s + " " + string y + "\n"               
                                         + childrenToS k (i+1) cs
and childrenToS k i = function | []    -> ""
                               | c::cs -> personToS k i c + childrenToS k i cs;;     


let toString k ft = personToS k 0 ft;;

// Q6 depth-first search
let rec truncate (P(n,s,y,cs)) = match s with 
                                 | F -> P(n,F,y,[])
                                 | M -> P(n,M,y,truncateChildren cs)
and truncateChildren cs = List.map truncate cs;; 

