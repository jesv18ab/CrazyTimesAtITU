

open System
open Microsoft.FSharp.Collections

type Heap<'a when 'a: equality> =
| EmptyHP
| HP of 'a * Heap<'a> * Heap<'a>;;

let ex3 = HP(8, HP(7, HP(6, EmptyHP, EmptyHP), EmptyHP), HP(5, HP(4, EmptyHP, EmptyHP), EmptyHP));;
// Type - Heap<int>
(*
This type is monomorphic. We can only store integers in the ex3 type, 
Had it been monomorphic, it would allow us to store various types of data in there. This is not the case
*)

exception HeapError of string
//This is how to call it:
(*
raise (MyFSharpError1 "message")
*)

let empty : Heap<'a> when 'a : equality = EmptyHP;;

let isEmpty h = if (h = empty) then true else false;;

//Be smart about, where the left and rigth tree is placed in the strcutures, in order for you to avoid acomplie erros.

(*
1.2 
Solution using tail recursion
*)
let size h =
    let rec inner h acc = 
        match h with
            |EmptyHP -> 0
            |HP (_, tl, tr) -> acc + ((inner tl acc ) + (inner tr acc))
    inner h 1       
            
//Tail recursion to find minimum. But this one can be optimized. For some reason I am having duplicate calls.
// But it works, as we just need the min val          
let find h =
    let rec inner h acc = 
        match h with
            |EmptyHP -> acc
            |HP (x, tl, tr) ->
                let left =  (inner tl ([x] @ acc))
                let right = (inner tr ([x] @ acc))
                (left @ right)
    (inner h [])|>List.min;;
let ex4 = HP(1, HP(2,HP(3,EmptyHP,EmptyHP),HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP))  

let extractMode x h =
    match h with
    |EmptyHP -> true
    |HP(r, _, _) -> x <= r

//Found online help for this one.
(*
Makes a lot of sense, though. The method takes a heap argument and starts a recursive function. 
For each traversal, we match that the current root i smaller or equal to the roots in each the nodes. 
*)

let rec chkHeapProperty h =
      match h with
        |EmptyHP -> true
        |HP(root, left, right) ->
            (extractMode root left) && (extractMode root right) && (chkHeapProperty left) && (chkHeapProperty right)

let rec map f h =
    match h with
    |EmptyHP -> EmptyHP
    |HP(root, tl, tr)  ->
        let tl = map f tl
        HP(f root, tl, map f tr);;

let tester = [0..10];;
let unPack g =
    match g with
    |(x::xs) -> List.sort x


let Conquer xs =
     let rec inner xs acc = 
        match xs with
        |[] -> acc
        |(x::xs) -> inner xs ((x @ acc) |> List.sort)
     inner xs []   
            
//The following steaps are taken - i did not follow the solution they suggested.
(*
We split the list in haft, a left and a right side.
Then we split bot sides until in the same amount of elements present in each list. 
Then we travers thorugh all lists and sort them along the way. 
Finally we merge the two lists - this merge ensures that the appropriate soring is being done 
*)            
let divideAndConquer (t) =
  match t with
  | t ->
        let splitted = List.splitInto 2 t
        let left = splitted.Head
        let right = splitted.Tail|>List.concat
        let leftFullSplit = List.splitInto left.Length left
        let rightFullSplit = List.splitInto right.Length right
        let sortLeft = Conquer leftFullSplit
        let sortRight = Conquer rightFullSplit
        let merged = (sortLeft @ sortRight)|>List.sort
        merged
        

//We make the sequence using the formula they suggested        
let triNum = Seq.initInfinite(
    fun i -> (i*(i+1))/2
    );;

//WE exploit the in built caching function to cache the sequence. 
let triNumC = triNum|>Seq.cache;;

//The filtering follows the following steps
(*
Index the cached trinum 
Map the k valu pairs and filter, such that only even numbers are stored. This is based on a modulo expression 

Finally, use map to store only the values extracted from the filtering process

There is definetly a better solutioon
*)
let filterOddIndex s = Seq.indexed s|>Seq.map( fun (k, v) -> (k,v))|>Seq.filter(fun(k,_) -> (k % 2 = 0))|>Seq.map(fun (_, v) -> v)


// We are making a sequence that takes every index from o ton infinity,
// We use each of these indices to extract the value related to that oncex in each of the soruce
// arguemtns to the function. These values are then stores as a tuples sequence lsit. 
let rec zipSeq s1 s2 = seq{ for i in Seq.initInfinite(fun i -> i) -> (Seq.item i s1, Seq.item i s2) }


exception FigError of string;;
type Point = P of double * double
type Fig =
    | Circle of Point * double
    | Line of Point * Point
    | Move of double * double * Fig
    | Combine of Fig list
    | Label of string * Fig
    | Ref of string;;
    
    
//Declaratio of a figure
(*
let figEx01 = Combine [Circle(P(1.0,1.0),2.0);Line(P(0.0,0.0),P(1.0,1.0))]
*)

let rectEx =    Combine [Line(P(-1.0, -1.0), P(1.0, -1.0));Line(P(1.0,1.0), P(1.0,-1.0)); Line(P(-1.0,1.0), P(1.0,1.0));Line(P(-1,-1), P(-1,1))];;
let p = Combine [Line(P(-2.0,1.0),P(1.0,1.0));Line(P(1.0,1.0),P(1.0,-1.0));
                Line(P(1.0,-1.0),P(-2.0,-1.0));Line(P(-2.0,-1.0),P(-2.0,1.0))]
let makeRect p1 p2 p3 p4 = Combine  [Line(p1, p3);Line(p3, p2); Line(p2, p4); Line(p4, p1)]

let rect (x1,y1) (x2,y2)  =
    let p1 = P(x1,y1)
    let p2 = P(x2,y2)
    match (p1, p2) with
    |(P(x1,y1), P(x2,y2) ) ->
        let p3 = P(x2, y1)
        let p4 = P(x1, y2)
        let makeFig = makeRect p1 p2 p3 p4
        (makeFig);;
        
  
  
let figEx02 = Combine [Label("c",Circle(P(0.0,0.0),1.0));
    Move(1.0,1.0,Ref "c");
    Move(2.0,2.0,Ref "c")];;


let rec buildEnv fig =
    match fig with
      | Combine figs ->
        let rec inner figs = 
            match figs with
            | Label(s, fig)::xs->  [(s, fig)]|>Map
            | Move(f, f1, fig)::xs-> buildEnv fig
            | Circle(point, f)::xs -> []|>Map
            | Line(point, point1) ::xs ->[]|>Map
            | Ref s ::xs -> []|>Map
            | _ -> failwith "error"
        inner figs    
      | _ -> failwith "Error"

let rec helperMethod fig =
    match fig with
      | Label(s, fig)-> [s,fig]
      | Move(f, f1, fig)-> helperMethod fig
      | Circle(point, f) -> []
      | Line(point, point1) ->[]
      | Combine x -> List.concat (List.map helperMethod x)
      | Ref s -> []
      | _ -> failwith "error"
let rec buildEnvTwo fig = helperMethod fig |>Map.ofList;;
printfn "%A" (buildEnv figEx02 )

(*
let rec substFigRefs (map: Map<string,Fig>) fig = 
    match fig with 
    | Circle (p,d) -> Circle (p,d)
    | Line (p1,p2) -> Line (p1,p2)
    | Ref x ->  match (map.TryFind x) with 
                | Some r -> substFigRefs map r
                | None -> Ref x
    | Label (s,f) -> match (map.TryFind s) with 
                        | Some r -> substFigRefs map r
                        | None -> substFigRefs map f
    | Move (d1,d2,f) -> Move(d1,d2, substFigRefs map f)
    | Combine x -> Combine (List.map (substFigRefs map) x);;
*)
let figEx03 = Combine [Label("c",Circle(P(0.0,0.0),1.0)); Label("a",Circle(P(0.0,0.0),1.0)); Move(2.0,2.0,Ref "c")];;


let envEx02 = buildEnv figEx03;;

let rec substFigRefs (map: Map<string,Fig>) fig = 
    match fig with 
    | Circle (p,d) -> Circle (p,d)
    | Line (p1,p2) -> Line (p1,p2)
    | Ref x ->  match (map.TryFind x) with 
                | Some r -> substFigRefs map r
                | None -> Ref x
    | Label (s,f) -> match (map.TryFind s) with 
                        | Some r -> substFigRefs map r
                        | None -> substFigRefs map f
    | Move (d1,d2,f) -> Move(d1,d2, substFigRefs map f)
    | Combine x -> Combine (List.map (substFigRefs map) x);;


let substEx02 = substFigRefs envEx02 figEx02
printfn "%A" substEx02;;

// Assuming label and references dosen't exists 
    let rec reduceMove fig =
        match fig with 
        | Circle (p,d) -> Circle (p,d)
        | Line (p1,p2) -> Line (p1,p2)
        | Combine x -> Combine (List.map (reduceMove) x)
        | Move (d1,d2,f) -> match f with 
                            | Circle (p,d) -> Circle (P(d1,d2),d)
                            | Line (p1,p2) -> Line (P(d1,d2),p2) // it is not very well formulated how a line should be moved? Here  we are just moving one end of the line. 
                            | Combine x -> Combine (List.map (reduceMove) x)
                            | _ -> failwith "Remove incomplete pattern match error"
        | _ -> failwith "Remove incomplete pattern match error"


    let reduceEx02 = reduceMove substEx02;;
