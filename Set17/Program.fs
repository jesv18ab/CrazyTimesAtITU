

open System
open System.Diagnostics.Metrics

type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>
let psEx = PrioritySet ["a";"b";"c"];;

//1.1
(*
No comment sneeded

type: PrioritySet<string>

Made  a terrible solution as weel - see below
*)
let priSetEx =
    let value = ["a";"q";"a";"b";"b";"q";"d";"a"]|>List.distinct|>PrioritySet
    value;;
let empty = PrioritySet[];;        
(*
    let tested = value.ToString()|>String.filter(fun i -> i<> '(' && i <> ')' && i <> ',' && i <> ';' && i <> ' ')
    let arr = tested.ToCharArray()|>Array.toList|>List.distinct|>PrioritySet
    arr;    
*)
let isEmpty = function
    |(PrioritySet l) -> if(l.IsEmpty = true) then true else false
    | (_) -> failwith "Error"


let size = function
    |(PrioritySet l) -> List.length l
    | (_) -> failwith "Error"

let contains e ps =
    match ps with
    |(PrioritySet ps) -> List.contains e ps
    | (_) -> failwith "Error";;

let getPriority l e =
    let rec f l e counter = 
        match l with
        | (x::l) -> if( x = e ) then counter else f l e (counter + 1)
        | [] -> failwith "Error"
    (f l e 1)
    

let getPN e ps =
    match ps with
    |((PrioritySet ps)) ->
        if (contains e (ps|>PrioritySet) = true) then getPriority ps e  else failwith "Error";;

let remove e ps =
    let prio = getPN e ps
    match ps with
    |((PrioritySet ps)) ->
        let newPS = ps|>List.removeAt (prio-1)
        (newPS|>PrioritySet)
let add e ps =
    if (contains e ps) then ps
    else match ps with
        |(PrioritySet ps) -> ps@[e]|>PrioritySet;;
        
let map f = function
    |(PrioritySet ps) -> ps|>List.map(f)|>PrioritySet;;
    
let taiRecursion g =
     let rec inner g acc = 
      match g with
      |(x::g) -> inner g (x@acc)
      |([]) -> acc
     inner g [];;
let cp ps1 ps2  = match (ps1, ps2) with
    | PrioritySet ps1, PrioritySet ps2 ->
                     let res = ps1|>List.map(fun i ->
                              match i with
                              |(i) ->
                              let cartesianProduct = ps2|>List.fold( fun acc v -> (i,v)::acc)[]
                              let tester = cartesianProduct |> Seq.toList
                              tester)
                     let formatted = taiRecursion res
                     (formatted|>PrioritySet);;
// we get a warning because we are only matching if there is a list with elements.
//We don't checker wheter the list contains more elements.
//I THINK - NOT SURE       
let f curRow =
    let rec f' = function
        |[] -> []
        | [_] -> [1]
        | xs ->
            let (x1::x2::xs) = xs
            let res = x1+x2
            res :: f' (x2::xs)
    (1 :: f' curRow);;
let fTail curRow =
    let rec f' curRow acc  =
        match curRow with
        |[] -> acc @ []
        | [_] ->  acc @ [1] 
        | (x1::x2::xs) ->
            f' (x2::xs) (acc @ [x1+x2])
    (1 :: f' curRow []);;

//I really struggled with this continuation, because we have to define new functionality of the continuation functionn.
(*
But we intiliazt the inner recursive functino with a list, containing 1
Then we match agains three clauses. Either the list is empty, where we return the empty list
Or the list has one argument, inn which case, we just append and concatenate a list, with the value one 
OR we take to elements from the original list and passes the sum of those to digits to the list. 
*)    
let fcont curRow =
    let rec fC curRow c =
        match curRow with
        |[] -> c []
        |(x1::x2::xs) -> fC (x2::xs) (fun i -> c(i @ [x2 + x1] ))
        |(_) -> c []@[1]
    ( 1::fC curRow id);;    

//Q
(*
This sequence computes a lister ordered after the cartesian product, not being the cartesian product
*)
let mySeq s1 s2 =
    seq { for e1 in s1 do
    for e2 in s2 do
    yield! [e1;e2] };;
let check = mySeq (["A";"B"]|>List.toSeq) (["D"; "E"; "F"])

//It is basically a double for loop, is this allowed? 
let mySeq2 s1 s2 =
    seq { for e1 in s1 do
    for e2 in s2 do
    yield! [(e1,e2)]}
    
 
let checker = mySeq2 ([1;2]|>List.toSeq) (["A"; "B"; "C"])

//The alterantive way needs to be tweeked a littlæ bit . Right now it computes a list of lists, but lists that con

//Something is wrong, don't know what
(* 
let alternative s1 s2  =
    match (s1) with
    |s1 ->s1
        |>Seq.toList|>List.map (fun i ->
                match i with
                |(i) ->
                let cartesianProduct = s2|>Seq.fold( fun acc v -> (i,v)::acc)[]
                let recu = cartesianProduct|>Seq.toList
                let make = taiRecursionTwo recu
                make
                )
            
            *)                 
//Be aware of this type conversion between ints and floats, they are tricky
// IF you want the power of a number, it needs to be of type float.
// However, you can not multiply a float with an int, so you need tp perform proper castings to achieve the result. 
let mysSeq3 n = Seq.initInfinite(
    let p = ((float n)**2)
    fun i -> (int p) - n * i
    );;
    
    
type DataSpec =
| RangeInt of int * int
| ChoiceString of string list
| StringSeq of string
| Pair of DataSpec * DataSpec
| Repeat of int * DataSpec
| Pick of string
| Label of string * DataSpec;;

let reg =
    [("a1",("cheese",25));
    ("a2",("herring",4));
    ("a3",("soft drink",5))];;



let pur = Repeat(2,Pair (RangeInt (0,10), StringSeq "a"));;

let rand = System.Random()
let next(i1,i2) = rand.Next(i1,i2)
let numGen =
    let n = ref 0
    fun () -> n := !n+1; !n;;
    
 let regTest =
    Repeat(3,Pair(StringSeq "a",
    Pair(ChoiceString["cheese";"herring";"soft drink"],
    RangeInt(1,100))));;
    
let rec genValue (ds:DataSpec) =
    match ds with
    | Repeat(i, dataSpec) -> (List.init i (fun _ -> genValue dataSpec)).ToString()
    | Pair(d1, d2) -> "(" + genValue d1 + genValue d2 + ")"
    | RangeInt(i1,i2) ->", " + next(i1,i2).ToString()
    | StringSeq s -> s.ToString()
    | ChoiceString l ->
        let arr = l|>List.toArray
        let found = arr[next(1,3)]
        found
    |_ -> "";;
    
let rec genValueTwo data =
    match data with 
    | RangeInt (i1,i2) -> string (next (i1,i2)) 
    | ChoiceString l -> l.[next(0,l.Length)]
    | StringSeq s -> s + string (numGen ())
    | Pair (d1,d2) -> "(" + genValue d1 + "," + genValue d2 + ")"
    | Repeat (i,d) -> (List.init i (fun non -> genValue d)).ToString ()
    | _ -> "" // Remove incomplete pattern match      


let pur2 = Repeat(2,Pair(RangeInt(1,10), Pick "articleCode"));;

type Env = Map<string,string list>;;

let addToEnv s v e =
 match e with
 | e -> if ((e|>Map.containsKey s))
            then
            let getList = (Map.find s e)
            let newList = getList @ [v]
            Map.remove s e |> Map.add s newList
            else e|>Map.add s [v]
let sample = Map [ ("a", ["1";"3";"4"]); ("g", ["4";"5";"6"]) ];;

let infinitesSeq = Seq.initInfinite(fun i -> i);;

let pickFromEnv v s e =
    match e with
    | e when (Map.containsKey s e ) = false -> failwith "Error"
    | e -> next(1,v).ToString();;
   
   
 let pickFromEnvTwo s (dEnv:Env) =
  match Map.tryFind s dEnv with
    Some vs -> List.item (next(0,List.length vs)) vs
  | None -> failwith "pickFromEnv: label does not exists in environment."
   

//Remeber this approach to updating a key that leads to a list in a map.
let addToEnvTwo name value (env: Env) = 
                    match (Map.tryFind name env) with 
                    | Some l -> env.Add (name, ( l @ [value] ) ) 
                    | None -> env.Add (name,[value]);;
 
 
 
 let reg2 = Repeat(3,Pair(Label("articleCode",StringSeq "a"),
    Pair(ChoiceString["cheese";"herring";"soft drink"],
    RangeInt(1,100))));;
 
let rec genValue_From_Niels dEnv = function
    RangeInt(i1,i2) -> (next(i1,i2+1).ToString(),dEnv)
  | ChoiceString xs -> let idx = next(0,List.length xs)
                       (List.item idx xs,dEnv)
  | StringSeq s -> (s + numGen().ToString(),dEnv)
  | Pair(ds1,ds2) ->
    let (v1',dEnv1) = genValue_From_Niels dEnv ds1
    let (v2',dEnv2) = genValue_From_Niels dEnv1 ds2
    ("("+v1'+","+v2'+")", dEnv2)
  | Repeat (n,ds) ->
    let rec iter (dEnv,vs) = function
        0 -> (dEnv,List.rev vs)
      | n when n>0 -> let (v',dEnv') = genValue_From_Niels dEnv ds
                      iter (dEnv',v'::vs) (n-1)
      | _ -> failwith "Repeat with n < 0"
    let (dEnv',vs') = iter (dEnv,[]) n                      
    ("[" + (String.concat ";" vs') + "]",dEnv')
  | Pick s -> (pickFromEnvTwo s dEnv,dEnv)
  | Label (s,ds) ->
    let (v',dEnv') = genValue_From_Niels dEnv ds
    (v', addToEnv s v' dEnv')
    
