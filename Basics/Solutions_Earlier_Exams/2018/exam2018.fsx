(*
    Question 1 
*)


type Heap<'a when 'a: equality> = 
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

let ex3 = HP(1, HP(2,HP(3,EmptyHP,EmptyHP),HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP))  

(*
    The type is Heap of either EmptyHP and HP. 
    It's polymorphic as Heap can either be EmptyHP or HP. 
*)

let emptyHeap = EmptyHP

exception HeapError of string
HeapError("Something went wrong")

let isEmpty h = if h = EmptyHP then true else false 


let rec count hp =
    match hp with 
    | EmptyHP -> 0
    | HP (value, l_hp, r_hp) -> 1 + count l_hp + count r_hp

 
let rec find h = 
    match h with 
    | EmptyHP -> failwith "Empty heap"
    | HP (value, l_hp, r_hp) -> value 


let findMin h : int = 
    let rec makeList h = 
        match h with 
        | EmptyHP -> []
        | HP (value, l_hp, r_hp) -> [value] @ makeList l_hp @ makeList r_hp
    List.min (makeList h)

findMin ex3    

// Version 1 
let chkHeapProperty h = 
    let rec HeapProperty h intialValue = 
        match h with 
        | EmptyHP -> true 
        | HP (value, l_hp, r_hp) -> (if value >= intialValue then 
                                        HeapProperty l_hp value 
                                                else false) 
                                                && (if value >= intialValue then 
                                                            HeapProperty r_hp value 
                                                            else false) 
    HeapProperty h (findMin h)

// Version 2   

let chkHeapProperty2 h = 
    let rec HeapProperty2 h intialValue = 
        match h with 
        | EmptyHP -> true 
        | HP (value, l_hp, r_hp) -> (if value >= intialValue then 
                                        (if (HeapProperty2 l_hp value && HeapProperty2 r_hp value) then true else false)
                                                else false) 
    HeapProperty2 h (findMin h)


let newHP = HP(1,HP(2,EmptyHP,EmptyHP),HP(3,EmptyHP,EmptyHP))

let test = HP(1,EmptyHP,EmptyHP)

let rec map f h = 
    match h with
    | EmptyHP -> h
    | HP (v,lh,rh) -> HP(f v,map f lh,map f rh)

map ((+) 1) ex3

let plus = (+) 1

let rec f n = if n % 2 = 0 then n - 10 else n + 10 

chkHeapProperty (map plus ex3)

chkHeapProperty (map f ex3)

(*
    it's traversing in pre-order 
*)



(*
Question 2 
*)

let random =
    let rnd = System.Random() 
    fun () -> rnd.Next(1,10000)

let genRandoms n = Array.init n (fun _ -> random ())

let genRandomsP n = Array.Parallel.init n (fun _ -> random ())

genRandomsP 4 

// let (|Even|Odd|) i = if i % 2 = 0 then Even else Odd

let split (xs: int list)  = if xs.IsEmpty then ([],[]) else (xs.GetSlice (None, Some ((xs.Length / 2)-1)), xs.GetSlice (Some (xs.Length / 2), None ))

split [342;535;21;1] // Even length - to test whether it splits the lists into two halfs 
split [1] // Odd length. To test whether is rounds up when splitting in half. To the second part has one more element.
split []  // Empty - To test whether it accepts an empty list. 


let indivisible xs = 
    match xs with 
    | [] -> true 
    | [x] -> true 
    | _ -> false 

let merge (xs,ys) : int list = (xs @ ys) |> List.sort // The question reqiures to sort w

merge ([1;3;4;5],[1;2;7;9])
merge ([12;12],[43;2]) // Elements in both 
merge ([],[43;2]) // One empty list 
merge ([],[]) // two empty lists


let rec divideAndConquer xs = 
    if indivisible xs then xs else let (l1, l2) = split xs 
                                   let left = divideAndConquer l1 
                                   let right = divideAndConquer l2
                                   merge (left, right)


divideAndConquer (Array.toList (genRandomsP 50))

// Question 3 

let triNum = Seq.initInfinite (fun n -> n*(n+1)/2)

let triNumC = Seq.cache triNum

let rec filterOddIndex s =
    Seq.append (Seq.singleton (Seq.item 0 s)) (Seq.delay (fun () -> filterOddIndex (Seq.skip 2 s)))

filterOddIndex triNumC

Seq.zip triNum triNum

let infi = Seq.initInfinite (fun n -> n)
let zipSeq s1 s2 =
    seq {for i in infi -> (Seq.item i s1, Seq.item i s2)}

zipSeq triNum triNum


// Question 4

exception FigError of string

type Point = P of double * double 

type Fig =
    | Circle of Point * double
    | Line of Point * Point
    | Move of double * double * Fig
    | Combine of Fig list
    | Label of string * Fig
    | Ref of string


let figEx01 = Combine [Circle(P(1.0,1.0),2.0);Line(P(0.0,0.0),P(1.0,1.0))]

let rectEx = Combine [Line(P(-1.0,1.0),P(1.0,1.0));Line(P(1.0,1.0),P(1.0,-1.0));Line(P(1.0,-1.0),P(-1.0,-1.0));Line(P(-1.0,-1.0),P(-1.0,1.0))]


let rect (x1,y1) (x2, y2) = Combine [Line(P(x1,y1),P(x2,y1)); Line(P(x2,y1),P(x2,y2)); Line(P(x2,y2),P(x1,y2));Line(P(x1,y2),P(x1,y1))] 

rect (-2.0,1.0) (1.0,-1.0)

let figEx02 = Combine [Label("c",Circle(P(0.0,0.0),1.0)); Move(1.0,1.0,Ref "c"); Move(2.0,2.0,Ref "c")]
let figEx03 = Combine [Label("c",Circle(P(0.0,0.0),1.0)); Label("a",Circle(P(0.0,0.0),1.0)); Move(2.0,2.0,Ref "c")]

// Version 1 
// let buildEnv (Combine x) = List.filter (fun x -> match x with 
//                                                     | Label (_,_) -> true
//                                                     | _ -> false ) x |> List.map (fun (Label (x,y)) -> (x,y) ) |> Map.ofList

let rec pairs fig = 
        match fig with 
        | Circle (p,d) -> []
        | Line (p1,p2) -> []
        | Ref s -> []
        | Move (d1,d2,f) -> pairs f
        | Combine x -> List.concat (List.map pairs x)
        | Label (s,f) -> [(s, f)]  

let buildEnv fig = pairs fig |> Map.ofList

let envEx02 = buildEnv figEx03

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
    | Combine x -> Combine (List.map (substFigRefs map) x)


let substEx02 = substFigRefs envEx02 figEx02

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


let reduceEx02 = reduceMove substEx02
