(* Example solutions to exam BFNP-F2014, june 2014 *)

(* Test Utilities *)
let doTest fns xs =
  List.map (fun fn -> List.map (fun x -> fn x) xs) fns |> Set.ofList |> Set.count = 1
let allTheSame l = List.forall (fun e -> e = List.head l) l

(* Assignment 1 - the code below contains more declarations than included in the exam set. *)
type OrderedList<'a when 'a : equality> = 
  {front: 'a list;
   rear: 'a list}

let ex = {front = ['x']; rear = ['z';'y']}

let ol1 = {front = ["Hans"; "Brian"; "Gudrun"]; rear = []}
let ol2 = {front = ["Hans"; "Brian"]; rear = ["Gudrun"]}
let ol3 = {front = ["Hans"]; rear = ["Gudrun"; "Brian";]}
let ol4 = {front = []; rear = ["Gudrun"; "Brian"; "Hans";]}
let ol = [ol1;ol2;ol3;ol4]

(* How many representations exists with the three elements in the givne order that fulfils the invariant?
   Answer 4. *)
  
let canonical = function
  {front=f; rear=r} -> {front = f @ List.rev r; rear = []}

let toList ol =
  match canonical ol with
    {front=f;rear=[]} -> f
  | _ -> failwith "toList: Invariant not fulfilled on canonical representation."
let _ = toList ex    

let p1_7 = List.map canonical ol
let p1_8 = allTheSame p1_7

let toString ol =
  match canonical ol with
    {front=f;rear=[]} -> String.concat "->" (List.map (fun e -> e.ToString()) f)
  | _ -> failwith "Canonical form error"
let p1_9 = List.map toString ol
let p1_10 = allTheSame p1_9

let newOL () = {front=[]; rear=[]}

let isEmpty = function
  {front=[];rear=[]} -> true
  | _ -> false
let p1_11 = isEmpty ex
let p1_12 = isEmpty (newOL())

let addFront e = function
  {front=f;rear=r} -> {front=e::f;rear=r}
let p1_13 = addFront 'w' ex

let removeFront = function
    {front=[];rear=[]} -> failwith "removeFront: Ordered list is empty."
  | {front=[];rear=r} -> let f = List.rev r
                         (List.head f, {front=List.tail f; rear=[]})
  | {front=f;rear=r} -> (List.head f, {front=List.tail f; rear=r})

let _ = removeFront ex
let p1_14 = (snd (removeFront p1_13)) = ex

let peekFront = function
    {front=[];rear=[]} -> failwith "peekFront: Ordered list is empty."
  | {front=[];rear=r} -> List.head (List.rev r)
  | {front=f;rear=_} -> List.head f

let p1_15 = peekFront ex = 'x'

let removeRear = function
    {front=[];rear=[]} -> failwith "removeRear: Ordered list is empty."
  | {front=f;rear=[]} -> let r = List.rev f
                         (List.head r, {front=[]; rear=List.tail r})
  | {front=f;rear=r} -> (List.head r, {front=f; rear=List.tail r})

let p1_16 = removeRear ex

let peekRear = function
    {front=[];rear=[]} -> failwith "peekRear: Ordered list is empty."
  | {front=f;rear=[]} -> List.head (List.rev f)
  | {front=_;rear=r} -> List.head r

let p1_17 = peekRear ex = 'c'
 
let addRear e = function
  {front=f;rear=r} -> {front=f;rear=e::r}

let p1_18 = addRear 'd' ex

let append ol1 ol2 =
  match (ol1, ol2) with
    ({front=f1;rear=r1},{front=f2;rear=r2}) -> {front=f1;rear=r1@List.rev f2@r2}

let p1_19 = toString (append ex ex)

let restrict e = function
  {front=f;rear=r} -> {front = List.filter (fun e' -> e<>e') f; rear = List.filter (fun e' -> e <>e') r}
let p1_20 = restrict 'b' ex

let restrictL ol1 ol2 =
  List.fold (fun acc e -> restrict e acc) ol1 (toList ol2)
let p1_21 = restrictL (addFront 'q' ex) ex

let map fn = function
  {front=f; rear=r} -> {front=List.map fn f;rear = List.map fn r}
let p1_22 = map (fun e -> e.ToString()) ex

let fold fn b ol = List.fold fn b (toList ol)
let p1_23 = fold (fun acc e -> acc + e.ToString()) "" ex

let multiplicity ol = 
  let addMult m e = 
    match Map.tryFind e m with
      Some i -> Map.add e (i+1) m
    | None -> Map.add e 1 m
  fold addMult Map.empty ol
let p1_24 = multiplicity (addFront 'x' ex)

(* Assignment 2 *)

let rec f i = function
    [] -> [i]
  | x::xs -> i+x :: f (i+1) xs

let _ = f 10 [0;1;2;3]
let _ = f 10 []

(* let N be the number of elements in the argument list [x0;..;xN-1].
   f computes the list [i+x0;(i+1)+x1;..;(i+N-1)+xN-1;i+N] *)
(* Can the result of f ever be the empty list? No *)
(* Can the function f ever go into an infinite loop? No *)

(* Make a tail recursive version of f *)
let fA i xs =
  let rec fA' i xs acc =
    match xs with
      [] -> List.rev (i::acc)
    | x::xs -> fA' (i+1) xs (i+x::acc)
  fA' i xs []

let fA2 i xs =
  let rec fA' i xs acc =
    match xs with
      [] -> acc@[i]
    | x::xs -> fA' (i+1) xs (acc@[i+x])
  fA' i xs []

let fC i xs =
  let rec fC' i xs c =
    match xs with
      [] -> c [i]
    | x::xs -> fC' (i+1) xs (fun acc -> c(i+x::acc))
  fC' i xs id

(*
Example call sequence
fC 3 [1;2] ==>
fC' 3 [1;2] id  ==>
fC' 4 [2] (fun acc -> id(4::acc))  ==>
fC' 5 [] (fun acc1 -> (fun acc -> id(4::acc))(6::acc1)) ==>
(fun acc1 -> (fun acc -> id(4::acc))(6::acc1))[5] ==>
(fun acc -> id(4::acc))(6::[5]) ==>
(fun acc -> id(4::acc))([6;5]) ==>
id(4::[6;5]) ==>
id([4;6;5]) ==>
[4;6;5]
*)

let _ = doTest [f 10;fA 10;fA2 10; fC 10] [[0;1;2;3];[]]

(* Assignment 3 *)

(* Notice the frist two questions are very much related. *)
let myFinSeq n M = Seq.map (fun m -> n+n*m) [0..M]
let _ = Seq.take 10 (myFinSeq 2 10)
(* The sequence myFinSeq n M to returns the elements n+n*m for m \in [0,...,M] *)

(* Define an infinite sequence mySeq n to return n+n*m for m \in [0,...] *)
let mySeq n = Seq.initInfinite (fun m -> n+n*m)
let _ = Seq.take 10 (mySeq 2)

(* Define the finite sequence multTable N M to return the tripples (n,m,n*m) where n \in [0,..M] and m \in [0,..M] *) 
let multTable N M = seq {for n in [0..N] do
                           for m in [0..M] do
                             yield (n,m,n*m) }
let _ = Seq.take 100 (multTable 10 10) 

(* Define the finite sequence ppMultTable N M to return the sequence "<n>*<m> is <n*m>" for n \in [0,..,N] and m \in [0,..,M],
   using the sequence multTable *)
let ppMultTable N M = Seq.map (fun (n,m,nm) -> sprintf "%d * %d is %d" n m (n*m)) (multTable N M)
let _ = Seq.take 4 (ppMultTable 10 10)


(* Assignment 4 - the code below contains more declarations than included in the exam set. *)

type opr = MovePenUp
         | MovePenDown
         | TurnEast
         | TurnWest
         | TurnNorth
         | TurnSouth
         | Step

type plot = Opr of opr
          | Seq of plot * plot
          | Repeat of int * plot
            static member (+) (p1,p2) = Seq(p1, p2) 

let side = Seq(Opr MovePenDown, Seq(Opr Step, Seq(Opr Step, Opr Step)))
let rect = Seq(Seq(Opr TurnEast, side), Seq(Opr TurnNorth, Seq(side, Seq(Opr TurnWest, Seq(side, Seq(Opr TurnSouth, side))))))
let side3 = Opr MovePenDown + Repeat(3, Opr Step)
let rect3 = Opr TurnEast + side3 + Opr TurnNorth + side3 + Opr TurnWest + side3 + Opr TurnSouth + side3
let side2 = Opr MovePenDown + Opr Step + Opr Step + Opr Step
let rect2 = Opr TurnEast + side2 + Opr TurnNorth + side2 + Opr TurnWest + side2 + Opr TurnSouth + side2

let ppOpr = function
    MovePenUp -> "MovePenUp"
  | MovePenDown -> "MovePenDown"
  | TurnEast -> "TurnEast"
  | TurnWest -> "TurnWest"
  | TurnNorth -> "TurnNorth"
  | TurnSouth -> "TurnSouth"
  | Step -> "Step"

let rec ppOprPlot = function
   Opr o       -> ppOpr o
 | Seq(p1, p2) -> ppOprPlot p1 + " => " + ppOprPlot p2
 | Repeat(n, p) -> String.concat " => " (List.replicate n (ppOprPlot p))

let _ = ppOprPlot rect

(* Write a function numOpr *)
let rec numOprPlot = function
   Opr _       -> 1
 | Seq(p1, p2) -> numOprPlot p1 + numOprPlot p2
 | Repeat(n, p) -> n * numOprPlot p

let _ = numOprPlot rect
let _ = numOprPlot rect2 
let _ = numOprPlot rect3
let _ = numOprPlot rect = numOprPlot rect2 && numOprPlot rect2 = numOprPlot rect3

(* Write a function numDots *)
(* Initially the mouse pen is up *)
let numDots p =
  let incDot down_p = function
      MovePenUp -> (0,false)
    | MovePenDown -> (1,true)
    | TurnEast -> (0, down_p)
    | TurnWest -> (0, down_p)
    | TurnNorth -> (0, down_p)
    | TurnSouth -> (0, down_p)
    | Step -> ((if down_p then 1 else 0), down_p)
  let rec numDots' p down_p =
    match p with
      Opr opr -> incDot down_p opr
    | Seq(p1, p2) -> let (n1, down_p) = numDots' p1 down_p
                     let (n2, down_p) = numDots' p2 down_p
                     (n1+n2,down_p)
    | Repeat(n, p) -> let (n1, down_p) = numDots' p down_p
                      (n*n1, down_p)
  numDots' p false

let _ = numDots rect
let _ = numDots rect2
let _ = numDots rect3
let _ = numDots rect = numDots rect2 && numDots rect2 = numDots rect3

type dir = North
         | South
         | West
         | East
type pen = PenUp
         | PenDown
type coord = int * int
type state = coord * dir * pen

let initialState = ((0,0),East,PenUp)

let goStep ((x,y),dir,pen) = 
  match dir with
     North -> ((x,y+1), dir, pen)
   | South -> ((x,y-1), dir, pen)
   | West  -> ((x-1,y), dir, pen)
   | East  -> ((x+1,y), dir, pen)

let s = goStep initialState

let addDot ((x,y),dir,pen) coords = function
    MovePenUp -> (coords,((x,y),dir,PenUp))
  | MovePenDown -> ((x,y)::coords,((x,y),dir,PenDown))
  | TurnEast -> (coords,((x,y),East,pen))
  | TurnWest -> (coords,((x,y),West,pen))
  | TurnNorth -> (coords,((x,y),North,pen))
  | TurnSouth -> (coords,((x,y),South,pen))
  | Step -> let (c,dir,pen) = goStep ((x,y),dir,pen)
            ((if pen=PenDown then c::coords else coords), (c,dir,pen))

let (coords1,s1) = addDot initialState [] MovePenDown
let (coords2,s2) = addDot s1 coords1 Step

let rec foldi fn acc i = 
  if i < 0 then acc else foldi fn (fn acc i) (i-1)   

let dotCoords p =
  let rec dotCoords' p (acc,((x,y),dir,pen)) =
    match p with
        Opr opr -> addDot ((x,y),dir,pen) acc opr
      | Seq(p1, p2) -> let (acc,((x,y),dir,pen)) = dotCoords' p1 (acc,((x,y),dir,pen)) 
                       dotCoords' p2 (acc,((x,y),dir,pen))
      | Repeat(n, p) -> foldi (fun acc _ -> dotCoords' p acc) (acc,((x,y), dir, pen)) (n-1)
  let (acc,_) = dotCoords' p ([],((0,0),East,PenUp))
  acc

let coords = dotCoords rect
let _ = dotCoords rect2
let _ = dotCoords rect3
let _ = dotCoords rect = dotCoords rect2 && dotCoords rect2 = dotCoords rect3

(* Write a version of dotCoords that returns the list of unique dots *)
let uniqueDotCoords p = dotCoords p |> Set.ofList

let coordSet = uniqueDotCoords rect
let _ = uniqueDotCoords rect2
let _ = uniqueDotCoords rect3
let _ = uniqueDotCoords rect = uniqueDotCoords rect2 && uniqueDotCoords rect2 = uniqueDotCoords rect3

let min x y = if x < y then x else y
let max x y = if x > y then x else y
let sizePlot p =
  let updMinMax (x:int,y:int) (minX, maxX, minY, maxY) =
    (min x minX,max x maxX, min y minY, max y maxY)
  let dots = uniqueDotCoords p
  let (minX, maxX, minY, maxY) = Set.foldBack updMinMax dots (0,0,0,0)
  (minX, minY, maxX-minX+1, maxY-minY+1)

let _ = sizePlot rect
let _ = sizePlot rect2
let _ = sizePlot rect3
let _ = sizePlot rect = sizePlot rect2 && sizePlot rect2 = sizePlot rect3

(* Write a function ppPlot *)
let ppPlot p =
  let nl = System.Environment.NewLine
  let dots = uniqueDotCoords p
  let (minX, minY, sizeX, sizeY) = sizePlot p
  let buildRow r =
    List.foldBack (fun c acc -> (if Set.exists (fun (c1,r1) -> r1=r && c1=c) dots then "*" else "-") + acc) [minX .. minX+sizeX-1] ""
  List.foldBack (fun r acc -> acc + (nl + buildRow r)) [minY .. minY+sizeY-1] nl 

let _ = ppPlot rect
let _ = ppPlot rect2
let _ = ppPlot rect3
let _ = ppPlot rect = ppPlot rect2 && ppPlot rect2 = ppPlot rect3

(* Write a function movePlot *)
let movePlot (x,y) p =
  let dir_x = if x < 0 then TurnWest else TurnEast
  let dir_y = if y < 0 then TurnSouth else TurnNorth
  let p_x = Opr dir_x + List.foldBack (fun _ acc -> Seq(Opr Step, acc)) [1 .. System.Math.Abs(x)] p
  let p_y = Opr dir_y + List.foldBack (fun _ acc -> Seq(Opr Step, acc)) [1 .. System.Math.Abs(y)] p_x
  Opr MovePenUp + p_y

let movePlot2 (x,y) p =
  let dir_x = if x < 0 then TurnWest else TurnEast
  let dir_y = if y < 0 then TurnSouth else TurnNorth
  let p_x = Opr dir_x + Repeat(System.Math.Abs(x),Opr Step) + p
  let p_y = Opr dir_y + Repeat(System.Math.Abs(y), Opr Step) + p_x
  Opr MovePenUp + p_y

let rect10 = movePlot (2,2) rect
let rect11 = movePlot2 (2,2) rect2 
let rect12 = movePlot2 (2,2) rect3
let rect13 = movePlot (-2,-2) rect
let rect14 = movePlot2 (-2,-2) rect2 
let rect15 = movePlot2 (-2,-2) rect3

let _ = ppPlot rect = ppPlot rect2 && ppPlot rect2 = ppPlot rect3

