// Solution to the exam set in 02157 Functional Programming
// 2013 Dec P3, 2018 May P1, 2017 May P2.1, 2016 May P3, 2016 May P4
//                                                

// 2013 Dec problem 3

type Title = string

type Section = Title * Elem list
and Elem = | Par of string 
           | Sub of Section

type Chapter = Title * Section list
type Book = Chapter list

let sec11 =
    ("Background",
     [ Par "bla"
       Sub(("Why programming", [ Par "Bla." ])) ])

let sec12 =
    ("An example",
     [ Par "bla"
       Sub(("Special features", [ Par "Bla." ])) ])

let sec21 =
    ("Fundamental concepts",
     [ Par "bla"
       Sub(("Mathematical background", [ Par "Bla." ])) ])

let sec22 =
    ("Operational semantics",
     [ Sub(("Basics", [ Par "Bla." ]))
       Sub(("Applications", [ Par "Bla." ])) ])

let sec23 = ("Further reading", [ Par "bla" ])
let sec31 = ("Overview", [ Par "bla" ])
let sec32 = ("A simple example", [ Par "bla" ])
let sec33 = ("An advanced example", [ Par "bla" ])
let sec41 = ("Status", [ Par "bla" ])
let sec42 = ("What’s next?", [ Par "bla" ])
let ch1 = ("Introduction", [ sec11; sec12 ])

let ch2 =
    ("Basic Issues", [ sec21; sec22; sec23 ])

let ch3 =
    ("Advanced Issues", [ sec31; sec32; sec33 ])

let ch4 = ("Conclusion", [ sec41; sec42 ])
let book1 = [ ch1; ch2; ch3; ch4 ]

//1 
let maxL = function
           | [] -> 0
           | xs -> Set.maxElement (Set.ofList xs)

maxL [1;3;4;6;2]

//2 
let rec overview = function
                   | []          -> []
                   | (t, _)::cs -> t::overview cs

overview book1

//3
let rec depthSection (t, es) = 1 + (depthElem es)
and depthElem = function
                | []           -> 0
                | (Par _)::es  -> depthElem es
                | (Sub s)::es  -> maxL ((depthSection s)::[(depthElem es)])

depthSection sec23
depthElem [ Sub(("Special features", [ Par "Bla."; Sub(("Special features", [ Par "Bla." ])) ])) ]

let rec depthChapter (t, ss) =
    match ss with
    | [] -> 1
    | s::ss -> maxL ((1+(depthSection s))::[(depthChapter (t, ss))])
depthChapter ch3

let rec depthBook = function
                    | [] -> 0
                    | c::cs -> maxL ((depthChapter c)::[depthBook cs])
depthBook book1

//
let tocB b=
    let rec tocC b i=  // Chapter
        match b with
        | [] -> []
        | (t, ss)::cs -> let ci = [i]
                         (ci,t)::(tocS ss ci 1)@(tocC cs (i+1))
    and tocS ss ci i =  // Section
        match ss with
        | [] -> []
        | (t, es)::st -> let si = ci@[i]
                         (si, t)::(tocE es si 1)@(tocS st ci (i+1))
    and tocE es si i =  // Element list
        match es with
        | [] -> []
        | (Par _)::es -> tocE es si i
        | (Sub (t, subes))::es -> let ei = si@[i]
                                  (ei, t)::(tocE subes ei 1)@(tocE es si (i+1))
    tocC b 1
tocB book1

// 2018 May P1

// Test map
let reg1 = Map.ofList [("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a3", ("soft drink", 5)); ("a3", ("bread", 8))]
// map [("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a3", ("bread", 8))]
let reg2 = Map.ofList [("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a4", ("bread", 8)); ("a3", ("soft drink", 5))]
// [("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a3", ("soft drink", 5)); ("a4", ("bread", 8))]
Map.map (fun ac (an,p) -> (an, int(round(0.85*(float p))))) reg2
Map.map (fun _ (an,p) -> (an, int(round(0.85*(float p))))) reg2
// Create a map
Map [("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a4", ("bread", 8)); ("a3", ("soft drink", 5))]
