

open Microsoft.FSharp.Collections

type OrderedListTwo<'a when 'a : equality> =
    {front: 'a list;
    rear: 'a list}


// Q1

// 1.1
(*
Remember form the text that the list needs to be kept in order in the case we do a operation like;
fron @ rear.reverse.

If we do that on ol1, then we get the original order;
original order: ["Hans"; Brian; Gudrun]

if we reverse rea in ol one and use @ for fron and rear, we get the initial order restored in a list
*)
let ol1 = {front = ["Hans"]; rear = ["Gudrun"; "Brian"]};;
let ol2 = {front = ["Hans"; "Brian"; ]; rear = ["Gudrun"]};;

// Q1

// 1.2
//We can take the initial value and make three difference orders, using the rear that withholds the invariant.
(*
let ol1 = {front = ["Hans"; Brian; gudrun]; rear = [};;
let ol2 = {front = ["Hans"; "Brian"; ]; rear = ["Gudrun"]};;
let ol3 = {front = ["Hans"]; rear = ["Gudrun"; "Brian"]};;
let ol4 = {front = []; rear = ["Gudrun"; "Brian";"Hans"]};;

*)

//1.2
(*
a
Function takes in an ordered list and uses a pipe to forward it as an argument to a function 
that takes the fron and rear element and computes a new front that is concatenated with the reversed order of the rear. 
The rear list is emptied

b)
Take a orderlist and concatenates the front with a reveresed order of the rear. 
Could have used this in a as well, but let's keep it to explore different ways of doing stuff.
 - Alternative solution is shown below the one presented here
*)
let ol4 = {front = []; rear = ["Gudrun"; "Brian";"Hans"]};;
let canonical ol = ol|> (fun {front = f; rear = r} -> {front = f @ List.rev r; rear = []});;
let toList ol =
    let list = ol.front @ List.rev(ol.rear)
    list;;
(*
let toList ol =
    let list = canonical ol|> (fun {front = f; rear = _} -> f )
    list;;
*)

//1.3
// No need for comments
let newOL = {front = []; rear = []};;
let isEmpty ol =
    if (ol.front.IsEmpty && ol.rear.IsEmpty)
    then true
    else false;;


//1,4
//No comments needed
let addFront element ol = {front = element::ol.front; rear = ol.rear};;
let newFront ol = toList {front = List.tail(ol.front); rear = ol.rear};;
let removeFront ol =
        if (ol.front.IsEmpty <> true)
        then
             let poppedItem = ol.front.Head
             (poppedItem, List.tail(ol.front))
        else failwith "No front present";;
        
//printfn "%A" ( removeFront {front = ["A"; "B"; "C"]; rear = []} )
let peekFront ol = ol.front.Head;;
let el  = {front = ["A"; "B"; "C"]; rear = []};;


//Super nice to be able to index stuff. Use this line another time
//printfn "%A" ( List.indexed el.front);;

//1.5
//I don't really get the point with this task. Whar are they trying to do even?
//Return to this whole motherfucking part. Code is fine, but descriptions are terrible. 
let append l1 l2 = { front= l1.front ; rear=l1.rear@ l2.front@l2.rear};;
//printfn "%A" (append { front=["x"]; rear=["z";"y"]} { front=["x"]; rear=["z";"y"]} );;        
      
//1.6

(*
This method includes a helper method that makes the list, we want to use, when applying the
function to each element. 
The idea is to keep the ordering from the initial list, even though, we 
move some element from the fron to the rear. 
*)

let makeListOfElement ol = [ol.front.Head] @ (ol.rear @ List.rev ol.front.Tail)

let map f ol = makeListOfElement ol |> List.map(fun i -> f(i)) |> (fun el -> {front = [el.Head]; rear= el.Tail});;

//1.7
//No comments needed
let fold ol = toList ol |> List.fold(fun acc e -> acc+e.ToString())"";;

//1.8
//I mean, we could just stop after bountBy id, but the task says it needs to be an
//Map<'a, int> so we have to perform the last step - retarded
let multiplicity ol = toList ol |> List.countBy id |> Map.ofList;; 


//Q2

(*
2.1

part one
This function takes an integer and a list as arguments
Then it performs a pattern matching. 
If the list is empty we return the list adding the incremeneted value from the inital list. At this point the integer could have been incremented severaæl times 
as seen in the example form the problem description. 

If the list is not empty we take the current head and adds it with the current integer argument and cons it to the recursively list we are building. 

part two. 
the type of this function is: int -> int list -> int list
We need to pass an integer. otherwise, we will encounter a compile error. For this reason, we can not return an empty list
as the return value will always be a list with an integer passed from the function call.

IF ANYONE IS READING THIS. I don't really care about these questions, so my answers are made without to much thought and are probably wrong most of the time. 

part three:
WE are recusively taking out element from a list. At some point this list will be empty
and we will terminate the loop. So, NO


*)
let rec f i = function
[] -> [i]
| x::xs -> i+x :: f (i+1) xs;;


// 2.2
//Tail recursive approach - add an accumulating list

let fA i xs =
    let rec fA' i acc = function
        [] -> acc@[i]
        | x::xs ->  fA' (i+1) xs (i+x :: acc)
    fA' i [];;
    
    
// 2.3
// Continuation based solution.  
let fC i xs =
    let rec fC' i xs c =
       match xs with  
        | [] -> c [i]
        | x::xs ->  fC' (i+1) xs (fun l -> c (i+x::l))
    fC' i xs id;;    
//printfn "%A" (f 10 [0;1;2;3])   


//Q3

(*
3.1

The function computes a sequence of integers 
that oncreases with the value of n until it reaches m amount of integers 
*)
let myFinSeq n M = Seq.map (fun m -> n+n*m) [0..M];;

// 3.2
let mySeq n = Seq.initInfinite(fun i -> if i >=0 then n+n*i else failwith "Not following zero constraint");;

//Spent much fucking time on this one, and then you can just use god damn ZIP - jeeeeez
//zip does exaclty what you want for this task. It takes to sources as arguments and pwir them one by one.
//We use this paring to create a final sequence with the multiplied value is applied, so the sequence consists of 3.tuples
let multTable N M =
    let l1 = [0..N]
    let l2 = [0..M]
    let seq = Seq.zip l1 l2 |> Seq.map(fun (i, i2) -> (i,i2, (i*i2)))
    (seq)

//3.4
// No comments needed
let ppMultTable N M = multTable N M |> Seq.map(fun (x, y, z) -> (x.ToString() + " * " + y.ToString() + " is " + z.ToString()));;


type opr = MovePenUp
         | MovePenDown
         | TurnEast
         | TurnWest
         | TurnNorth
         | TurnSouth
         | Step

type plot = Opr of opr
          | Seq of plot * plot;;
let ppOpr = function
  | MovePenUp -> "MovePenUp"
  | MovePenDown -> "MovePenDown"
  | TurnEast -> "TurnEast"
  | TurnWest -> "TurnWest"
  | TurnNorth -> "TurnNorth"
  | TurnSouth -> "TurnSouth"
  | Step -> "Step";;

let side = Seq(Opr MovePenDown, Seq(Opr Step, Seq(Opr Step, Opr Step)))
let rect = Seq(Seq(Opr TurnEast, side),
    Seq(Opr TurnNorth, Seq(side,
    Seq(Opr TurnWest, Seq(side,
    Seq(Opr TurnSouth, side))))));;
  
  
//So what heppens actually? - Had a lot of help from the solutions on this one.
(*
  We take a plot as an argument. At this point a plot can either be a sequence plot tuples or an operator wit potential more sequences of operators
  For each of the tuples we need to expand the recursion so that wer can retrieve all operators.
  When we encounter an operator we converts it to a string using the function, constrcuted earlier
  *)  
  
  let rec ppOprPlot plot =
      match plot with
      | Opr operator -> ppOpr operator
      | Seq(plot1, plot2) -> ppOprPlot plot1 + " => " + ppOprPlot plot2;;
      
type dir =
    | North
    | South
    | West
    | East;;

type pen =
    | PenUp
    | PenDown;;

type coord = int * int;;
type state = coord * dir * pen;;

let initialState = ((0,0),East,PenUp)

let goStep stateChange =
    match stateChange with
    |((v1,v2), East, PenUp) ->  ((v1+1, v2), East, PenUp)
    |((v1,v2), East, PenDown) -> ((v1+1, v2), East, PenDown)
    |((v1,v2), West, PenUp) -> ((v1-1,v2), West, PenUp)
    |((v1,v2), West, PenDown) -> ((v1-1,v2), West, PenDown)
    |((v1,v2), South, PenUp) -> ((v1,v2-1), South, PenUp)
    |((v1,v2), South, PenDown) -> ((v1,v2-1), South, PenDown)
    |((v1,v2), North, PenDown) -> ((v1,v2+1), North, PenDown)
    |((v1,v2), North, PenUp) -> ((v1,v2+1), North, PenUp);;

let addDot state coordList opr =
    let startCoord = state |> (fun ((v1,v2), _, _) -> (v1,v2)) 
    let newState = goStep state
    if(opr = MovePenDown || state|>(fun (_, _, v) -> v) = PenDown ) then (startCoord::coordList, state) else (coordList, newState);;
let (coords1,s1) = addDot initialState [] MovePenDown;;
let (coords2,s2) = addDot s1 coords1 Step;;

printfn "%A" coords1;;
printfn "%A" s1;;
printfn "%A" coords2;;
printfn "%A" s2;;
//printfn "%A" (addDot initialState [] MovePenDown);;     
    
      
