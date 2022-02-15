//week5
open System
type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS
type CourseBase = Map<CourseNo, CourseDesc>
type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional
type BasicNaturalScience = CourseGroup
type TechnologicalCore = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective = CourseNo -> bool
type FlagModel = BasicNaturalScience*TechnologicalCore*ProjectProfessionalSkill*Elective
type CoursePlan = Set<CourseNo>
//2015 summer
//1.
let isValidCourseDesc desc =
    match desc with
    | (_, ects) when ects % 5 = 0 -> true
    | (_, ects) -> false

(*isValidCourseDesc ("Computer Science Modelling", 5)*)
//2.
let isValidCourseBase cb =
    Map.forall (fun _ desc -> isValidCourseDesc desc) cb

(*isValidCourseBase (Map.ofList [ 2141, ("Computer Science Modelling", 4) ])*)
//3.
let disjoint s1 s2 =
    if (Set.intersect s1 s2) = Set.empty then
        true
    else
        false

(*disjoint (set [ 2131; 2141 ]) (set [ 2157; 2158 ])*)
//4.
let sumECTS cs cb =
    let p no cb = Map.containsKey no cb
    // let (no, ects) = Map.find no cb
    Set.fold
        (fun t no ->
            let (title, ects) = Map.find no cb
            if p no cb then ects + t else t)
        0
        cs

(*sumECTS
    (set [ 2131; 2141 ])
    (Map.ofList [ (2131, ("Embedded System", 5))
                  (2141, ("Computer Science Modelling", 10)) ])*)
//5.
let isValidCourseGroup cg cb =
    let (man,opt) = cg
    let sumopt = sumECTS opt cb
    let summan = sumECTS man cb
    disjoint man opt
    && (summan < 45 || (summan = 45 && Set.count opt = 0))
    && summan + sumopt >= 45

(*isValidCourseGroup
    (set [ 2131 ], set [ ])
    (Map.ofList [ (2131, ("Embedded System", 45))
                  (2141, ("Computer Science Modelling", 45))
                  (2157, ("Founctional programming", 5))
                  (2158, ("Parallel programming", 5))])*)
//6.
let union (man,opt) = Set.union man opt
let ep no = no > 2000
let isValid fm cb = 
    let (bns,tc,pps,ep) = fm
    let allbns = union bns
    let alltc = union tc
    let allpps = union pps
    let allcourses = Set.union (Set.union allbns alltc) allpps
    printfn "run"
    isValidCourseGroup bns cb
    && isValidCourseGroup tc cb
    && isValidCourseGroup pps cb
    && Set.count allbns + Set.count alltc + Set.count allpps = Set.count allcourses
    && Set.forall ep allcourses
(*isValid 
    ( (set [ 2131 ], set [ ]),(set [ 2141 ], set [2157 ]),(set [ 2158 ], set [1006 ]),ep ) 
    (Map.ofList [ (2131, ("Embedded System", 45))
                  (2141, ("Computer Science Modelling", 40))
                  (2157, ("Founctional programming", 5))
                  (2158, ("Parallel programming", 35))
                  (2801, ("Introduction to Artificial Intelligence",5))
                  (1006, ("Advanced Engineering mathematics1",10))])*)
//7.
let checkPlan cp fm cb = 
    let (bns,tc,pps,ep) = fm
    sumECTS cp cb = 180
    && sumECTS (union bns) cb = 45
    && sumECTS (union tc) cb = 45
    && sumECTS (union pps) cb = 45
checkPlan 
    ( set [2131;2141;2157;2158;1006;2800] )
    ( (set [ 2131 ], set [ ]),(set [ 2141 ], set [2157 ]),(set [ 2158 ], set [1006 ]),ep ) 
    (Map.ofList [ (2131, ("Embedded System", 45))
                  (2141, ("Computer Science Modelling", 40))
                  (2157, ("Founctional programming", 5))
                  (2158, ("Parallel programming", 35))
                  (2801, ("Introduction to Artificial Intelligence",45))
                  (1006, ("Advanced Engineering mathematics1",10))])

