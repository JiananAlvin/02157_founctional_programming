// week6
open System
type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list
type ArrivalCatalogue = (Airport * Lid list) list
// 1
let lc =
    [ ("DL 016-914",
       [ ("DL 189", "ATL")
         ("DL 124", "BRU")
         ("SN 733", "CPH") ])
      ("SK 222-142",
       [ ("SK 208", "ATL")
         ("DL 125", "BRU")
         ("SK 122", "JFK") ])
      ("SK 222-142",
       [ ("SK 208", "ATL")
         ("DL 124", "BRU")
         ("SK 122", "JFK") ]) ]

let rec findRoute (lid1, lc) =
    match lc with
    | (lid, route) :: lt when lid1 = lid -> route
    | (lid, route) :: lt -> findRoute (lid1, lt)
    | _ -> failwith ("No such a piece of luggage")

// findRoute ("SK 222-143", lc)

// 2
let route =
    [ ("DL 189", "ATL")
      ("DL 124", "BRU")
      ("SN 733", "CPH") ]

let rec inRoute flight1 (route: Route) =
    match route with
    | (flight, airport) :: rt when flight1 = flight -> true
    | (flight, airport) :: rt -> inRoute flight1 rt
    | [] -> false

//inRoute "DL 123"  []

// 3
let rec withFlight f lc =
    match lc with
    | (lid, route) :: lt when inRoute f route -> lid::(withFlight f lt)
    | (lid, route) :: lt -> (withFlight f lt)
    | [] -> []

withFlight "DL 124" lc
// withFlight "SK 122" lc

// 4
let rec insert lid airport ac = 
    match ac with
    | [] -> [(airport,[lid])]
    // if find the airport, add the lid in the airport done
    | (ap,lids)::act when ap=airport -> (ap,lid::lids)::act //lids is lid_list
    // if dom't find the airport, keep head, and then check the tail
    | (ap,lids)::act -> (ap,lids)::(insert lid airport act)

let rec extend (lid, r, ac) =
    match r with 
    | [] -> ac
    // We need a help function that adding lid into corresponding airport in ac
    | (flight,airport)::rt -> extend (lid, rt, (insert lid airport ac))

let ac = [("ATL", ["SK 222-142"]);
          ("BRU", ["SK 222-142"]);
          ("JFK", ["SK 222-142"]);]

extend ("DL 016-914",[("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")],[])

// let result1 = extend ("DL 016-914", route, ac)