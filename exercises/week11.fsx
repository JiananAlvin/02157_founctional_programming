//week 11
// 11.1
let odd = Seq.initInfinite (fun i -> 2 * i + 1)
Seq.item 4 odd

// 11.2
let rec factor =
    function
    | 0 -> 1
    | i -> i * (factor (i - 1))

let factorSeq = Seq.initInfinite factor
Seq.item 3 factorSeq

// 11.3
let rec factorSeq2 i previous =
    seq {yield (i*previous)
         yield! factorSeq2 (i + 1)  (i*previous)
    }
let factorSq = seq {yield 1
                    yield! (factorSeq2 1 1)}
Seq.item 3 factorSq

// 11.9
let rec enumeration i =
    seq {yield! [-i; i]
         yield! (enumeration (i + 1))}
let rec enumerationSq = seq {yield 0
                             yield! (enumeration 1)}
Seq.item 2 enumerationSq
