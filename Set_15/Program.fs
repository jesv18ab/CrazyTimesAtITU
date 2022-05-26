

open System
open System.ComponentModel
open System.Data
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type multimap<'a,'b when 'a: comparison> =
    MMap of Map<'a,list<'b>>;;

let ex = MMap (Map.ofList [("record",[50]);("ordering",[36;46;70])]);;


(*
Q1

1.1: No comments needed
*)
let studReg = MMap (Map.ofList [("Grete", [])
                                ("Hans", ["Tops";"Hops"])
                                ("Peter", ["IFFY"])
                                ("Sine", ["BFNP"; "HOPS"; "IFFY"])
                                    ]);;
let studRegUnsorted = MMap (Map.ofList [("Grete", [])
                                        ("Peter", ["IFFY"])
                                        ("Thomas", ["IFFY"])
                                        ("Sine", ["BFNP"; "HOPS"; "IFFY"])
                                    ]);;

(*
Part two:
Not sure what they are looking for
If I change a key, then the same course registrations will ofcause not simplify to true, when compares to studreg
Is that why, A has to be comparable?
*)
let studRegTwo = MMap (Map.ofList [("Grete", [])
                                   ("Hans", ["TOPS;HOPS"])
                                   ("P", ["IFFY"]);
                                   ("Thomas", ["BFNP"; "HOPS"; "IFFY"])
                                   ("Sine", ["BFNP"; "HOPS"; "IFFY"])
                                   ]);;
//printfn "%A" (studReg = studRegTwo);;


//Part three
(*
Sorting functions works just fine. First we sort values for each entry. 
Next we sort by key. In built functions are great
*)
let canonical = function
    | MMap aMap ->
        let values = aMap |> Seq.map(fun (k) -> (k.Key, List.sort k.Value))|> Seq.sortBy id
        (values);;
//printfn "%A" (canonical studRegUnsorted);;

(*
Nothing is stated with regard to distinct values in the list. 
However, i assume that this is want they want. For this reason the List.distinct operator is used here.
It can just be removed, which would make it simplify to (4,6)

Fuck that, removed distinct, but you can just have this line, if you want i
Map.values |> Seq.toList |> List.concat |> List.distinct|> List.length
*)

let makeListFromValues k m = m |> Map.find k |> Seq.toList;;


let toOrderedList studReg = canonical studReg |> Seq.toList;

let newMultimap = MMap (Map.empty);;
let sizeMultimap studReg =
    match studReg with
    |(MMap aMap) ->
        let keys = aMap |> Map.keys |> Seq.toList |> List.length
        let values = aMap |> Map.values |> Seq.toList |> List.concat |> List.length
        (keys, values)
    | _ -> failwith "Error";;    


(*
So this was never s nice and sophisticated solution,  but it works with right type. 
*)


let addMultimap k v m =
    match m with
    |MMap aMap when (aMap|>Map.containsKey k ) = true ->
            let list = aMap |>Map.find k
            let updatedList = v::list |> List.distinct
            let valAdded = aMap |> Map.add k updatedList |> MMap
            valAdded
    |MMap aMap when (aMap|>Map.containsKey k) <> true ->
        let newList = v ::[]
        let valAdded = aMap |> Map.add k newList |> MMap
        valAdded
    | _ -> failwith "Error";;

//Not sure what they want me to do here. But the function for sure,
//Removes the value related to a key, in the case it exists. Otherwise, we make sure to add the key and then remove it again  - retarded.
//Yeah okay, if the value does notexists, just don't try and remove it. Makes sense. I was the retarded one in this case


let changeValInMap k v m =
            let list = m |>Map.find k |> Seq.toList|>List.filter(fun i -> i <> v )
            let removeItem = m |>Map.remove k
            let updated = removeItem |> Map.add k list |> MMap
            (updated)
            
            
let rec removeMultimap k vOpt m = 
    match m, vOpt  with
    |((MMap aMap), Some v) when Map.containsKey k aMap <>true -> m
    |((MMap aMap), None) -> aMap |> Map.remove k |> MMap
    |((MMap aMap), Some v) ->
        let multimap = changeValInMap k v aMap
        multimap
    | _, _ -> failwith "Error"

//let rec makeList k v = v |>  

(*
RETURN TO THIS SHIT
*)
let mapMultimap f m =
    match m with
    |MMap aMap ->
        let values =
                    let keysTuples = aMap|>Map.map ( fun k v -> (
                                      match k, v with
                                      |(k, v) -> v|>List.map( fun i -> (k, i))))
                    let keysRemoved = keysTuples|> Map.values
                                      |> List.concat|>List.map f
                    (keysRemoved)
        let keys = values |> List.groupBy fst|> List.map(fun ( k, v) -> k, match (v) with
                                                         | (v) -> v|> List.map(fun (_,v) -> v))
        let toMap = Map.ofList values
        let keysToMap = Map.keys toMap |>Seq.toList
        let originalKeys = Map.keys aMap|>Seq.toList
        let notIncluded = ((Set.ofList originalKeys) )- (Set.ofList keysToMap)
        let addValues = notIncluded |> Set.map( fun key -> (aMap |> Map.find key) |> ( fun (v) -> (Map.add key "[]") toMap ))  
        (addValues);;
let travers s = s|> List.map (fun (k, v) -> (k, "hej" ))
//printfn "%A" (mapMultimap (fun (k, v) -> (k,(v+"-F2015"))) studReg);;

//Q2

//2.1
(*
This function has three arguments; 
two intergers, i and j, and a list of integers.
It is a recursive function, wherein we keep extracting values from the list and cons them into the list we are building recursively.

The values we include are ranging between positive and negative integers, as we are multiplying j with -1 and i is multiplied with j 
 when for each iteration(don't this word in the functional world). 

It can never be the empty list, as no of the return values from the function defines an empty list.

*)
let rec f i j xs =
    if xs = [] then
    [i*j]
    else 
        let (x::xs') = xs
        x*i :: f (i*j) (-1*j) xs';;        

let fA i j xs =
  let rec fA' i j xs acc = 
   match xs with 
   | [] -> acc @ [i*j] 
   | x::xs' -> fA' (i*j) (-1*j) xs' (acc @ [x*i])
  fA' i j xs []                    

let fAC i j xs =
    let rec fAC i j xs c = 
     match xs with 
     | [] -> c [i*j] 
     | x::xs' -> fAC (i*j) (-1*j) xs' ( fun k -> c ( [x*i] @ k))
    fAC i j xs id;;  
    
//printfn "%A" (f 10 1 [1 .. 9]) ;; 
//printfn "%A" (fAC 10 1 [1 .. 9]);; 
 
 (*
 The sequence returns a list of lists. Each lists expands with one element pr iteration. 
 The elements in the lists are going from n to M. 
 In this particular list, 12 will occur in three lists
 
 
 
 *)
let myFinSeq n m = seq { for i in [n .. m] do
yield [n .. i] };;

//printfn "%A" (myFinSeq 10 14)
    
let myFinSeq2 n m = myFinSeq n m|>Seq.concat;;

let sum xs = List.fold (fun r x -> r+x) 0 xs;;
let seq4000 = myFinSeq 10 4000;;
let array4000 = Array.ofSeq seq4000;;

//num arrays = 1991
printfn "%A" (array4000|> Seq.length);;
let sums = Array.map sum array4000
let sumsParallel = Array.Parallel.map sum array4000;;


//Q4 - Not today                    
                             
                             