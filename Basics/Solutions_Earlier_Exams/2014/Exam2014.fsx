type OrderedList<'a when 'a : equality > = {front: 'a list; rear: 'a list}

// Q 1.1
let ex = {front = []; rear = ['x';'z';'y']}
let ol1 = {front = ["Hans"; "Brian"; "Gudrun"]; rear = []}
let ol2 = {front = []; rear = ["Gudrun"; "Brian"; "Hans"]}
let ol3 = {front = ["Hans" ]; rear = ["Gudrun"; "Brian"]}
let ol4 = {front = ["Hans";"Brian" ]; rear = ["Gudrun"]}


(*
    there exist 4 representations with 3 element in the OrderedList<String>.  
*)


// Q 1.2 

let rec canonical l = 
    match l with 
    | {OrderedList.rear = []} -> l
    | {front = x; rear = y} -> canonical {front = x @ (List.rev y) ; rear = []}


let toList l = canonical l |> fun ({front = x; rear = y}) -> (x)

toList ol4


// Q 1.3
let newOL () = {front = []; rear = []}

let isEmpty = function 
    | {front = []; rear = []} -> true 
    | _ -> false 

isEmpty (newOL())
isEmpty ol4

// Q 1.4
let addFront e (l: OrderedList<'a>) = {front = e::l.front; rear = l.rear}

addFront 'a' ex

exception EmptyList 
let ValidList l = if l = [] then raise EmptyList else l

let removeLast x = List.rev x |> fun (x) -> (x.Tail) |> List.rev

exception EmptyOrderedList
let removeFront l = 
    if isEmpty l = true then raise EmptyOrderedList  
    else match l with 
            | {front = []; rear = x} -> (x.[x.Length-1],{front = []; rear = removeLast x  })
            | {front = x::xs; rear = y} -> (x,{front = xs; rear = y})
            
removeFront ol3

let peekFront l = 
    if isEmpty l = true then raise EmptyOrderedList  
    else match l with 
            | {front = []; rear = x} -> x.[x.Length-1]
            | {front = x::xs; rear = y} -> x

peekFront ol3

// Q 1.5
let append l1 l2 = toList l1 @ toList l2 |> fun (x) -> ({front = []; rear = x})

// Q 1.6 
let map f = fun ({front = front; rear = rear}) -> 
        let l_front = List.map f front 
        let r_rear = List.map f rear
        {front = l_front; rear = r_rear}

// Q 1.7 
let fold f s l = toList l |> List.fold f s 

fold (fun acc e -> acc + e.ToString()) "" ex

// Q 1.8 
let multiplicity l = 
    let add m e = 
        match Map.tryFind e m with 
        | Some i -> Map.add e (i+1) m
        | None -> Map.add e 1 m
    fold add Map.empty l 


multiplicity ex

let a = [1;2;3;4;5;6;7]
List.map (fun (x) -> (x+1)) a

a.GetSlice (Some 0, Some a.[a.Length-2])

// Q2


(* Q.2.1 

    It creates a new list based on the value of 'i' plus each element in the input list. 
    'i' increments by one for each iteration.
    In the base case it adds the value of i.

    So starting with 'i' = 10 
    and a list [0;1;2;3]
    
    10+0 = 10 
    11+1 = 12 
    12+2 = 14 
    13+3 = 16
    14 = 14 

   For each element in the input list, 'i' is incremented by 1 and plussed with each element in the input list. 
   Finally the value of i is added to the list. 

   it can never be an empty list, as the base case wil always output 'i' 

   I tried to convert an infinite sequence to a list, but that didn't work. 
   So my conclusion is that a list is finite and therefore it can never run forever. 

*)

// Q 2.2 


let rec f i list  =  
    match list with
    |[] -> [i]
    | x::xs -> i+x :: f (i+1) xs


let funcTail i list =
    let rec tail i list acc = 
        match list with
        |[] -> acc @ [i]
        | x::xs -> tail (i+1) xs (acc @ [x+i])
    tail i list []

funcTail 10 [0;1;2;3] 

// Q 2.3

let rec fo i list  =  
    match list with
    |[] -> [i]
    | x::xs -> i+x :: fo (i+1) xs


let rec fCon i list f  =  
    match list with
    |[] -> f [i]
    | x::xs -> fCon (i+1) xs (fun s -> f(x+i::s))  

fCon 10 [0;1;2;3] (fun s->s)


// 3.1 

let myFinSeq n M = Seq.map (fun m -> n+n*m) [0..M]

myFinSeq 7 3

(*
    2+2*0 = 2 
    2+2*1 = 4 
    2+2*2 = 6 

    The generated sequence is similiar to a multiplication table for defined value of n.   
    The sequence adds 7 to each next element. 
*)
// 3.2 
let infi n = Seq.initInfinite (fun i -> n+n*i)

// 3.3 


let multTable N M = 
    seq { for n in 0..N do 
                for m in 0..M do yield (n,m,n*m)  }
 


Seq.take 4 (multTable 5 5)
// 3.4 

let ppMultTable n m = Seq.map (fun (a,b,c) -> string a + " * " + string b + " is " + string c) (multTable n m)

Seq.take 4 (ppMultTable 10 10)

// Q 4 

type opr = 
        | MovePenUp
        | MovePenDown 
        | TurnEast
        | TurnWest
        | TurnNorth
        | TurnSouth
        | Step


type plot = 
        | Opr of opr
        | Seq of plot * plot
        | Repeat of int * plot
        static member (+) (p1,p2) = Seq (p1,p2)

let sideTypeAugmentation = Opr MovePenDown + Opr Step + Opr Step + Opr Step

let side = Seq(Opr Step, Seq(Opr Step, Seq(Opr Step, Opr Step))) 

let rect = Seq(Seq(Opr MovePenDown, side),
               Seq(Opr TurnNorth, Seq(side,
                    Seq(Opr TurnWest, Seq(side,
                            Seq(Opr TurnSouth, side))))))

let ppOpr opr = 
    match opr with 
    | MovePenUp -> "MovePenUp"
    | MovePenDown -> "MovePenDown"
    | TurnEast -> "TurnEast"
    | TurnWest -> "TurnWest"
    | TurnNorth -> "TurnNorth"
    | TurnSouth -> "TurnSouth"
    | Step -> "Step"

let rec ppOprPlot plot = 
    match plot with
    | Opr d -> string d
    | Seq (d1, d2) -> ppOprPlot d1 + " => " + ppOprPlot d2

ppOprPlot rect

// Q 4.2 

type dir= 
    | North
    | South
    | West
    | East

type pen = 
    | PenUp
    | PenDown

type coord = int * int

type state = coord * dir * pen

let initialState = ((0,0),East,PenUp)

let goStep = fun (((x,y),dir,pen) : state ) -> match dir with 
                                                | North -> state (coord (x,y+1),dir,pen)
                                                | South -> state (coord (x,y-1),dir,pen)
                                                | West -> state (coord (x-1,y),dir,pen)
                                                | East -> state (coord (x+1,y),dir,pen)


goStep initialState


let getNewCoor (state: state) =  goStep state |> fun (((x,y),dir,pen) :state) -> (x,y)  

let addDot (((x,y),dir,pen): state) (coord_list: coord list) (opr: opr) =
            match pen with 
            | PenDown ->
                 match opr with 
                    | MovePenUp -> (coord_list, goStep ((x,y),dir,PenUp))
                    | MovePenDown -> failwith "Pen is already down"
                    | TurnEast -> (coord_list, state ((x,y),East,pen))
                    | TurnWest -> (coord_list, state ((x,y),West,pen))
                    | TurnNorth -> (coord_list, state ((x,y),North,pen))
                    | TurnSouth -> (coord_list, state ((x,y),South,pen))
                    | Step -> (coord_list @ [getNewCoor ((x,y),dir,pen)], goStep ((x,y),dir,pen))
            | PenUp -> 
                match opr with 
                    | MovePenUp -> failwith "Pen is already up"
                    | MovePenDown -> (coord_list @ [(x,y)], state ((x,y),dir,PenDown))
                    | TurnEast -> (coord_list, goStep ((x,y),East,pen))
                    | TurnWest -> (coord_list, goStep ((x,y),West,pen))
                    | TurnNorth -> (coord_list, goStep ((x,y),North,pen))
                    | TurnSouth -> (coord_list, goStep ((x,y),South,pen))
                    | Step -> (coord_list, goStep ((x,y),dir,pen))
          

let (coords1,s1) = addDot initialState [] MovePenDown 
let (coords2,s2) = addDot s1 coords1 Step
let (coords3,s3) = addDot s2 coords2 TurnNorth
let (coords4,s4) = addDot s3 coords3 Step

let rec dotCoords plot = 
    let rec makeList plot (list,(x,y),dir,pen) = 
        match plot with 
        | Opr (opr) -> addDot ((x,y),dir,pen) list opr
        | Seq(p1, p2) -> let ((list),((x,y),dir,pen)) = makeList p1 (list,(x,y),dir,pen)
                         makeList p2 (list,(x,y),dir,pen)
        
    let (acc,_) = makeList plot ([],(0,0),East,PenUp) 
    acc

// let rec listCoord list opr = list @ listCoord addDot list opr 
//                    let (coords,s) = listCoord initialState [] opr 
//                    coords


let side2 = Seq(Opr MovePenDown, Opr Step) 
let test = dotCoords rect
// Found error in assignment, dosen't make sense to put pen down, which is already down. 

let uniqueDotCoords plot = Set.ofList (dotCoords plot)



// 4.3 


// side2 + side2


// type plot = static member ( + )  : plot * plot -> plot

//   static member ( + ) (Seq(x1,y1),Seq(x2,y2)) = Seq(x1+x2,y1+y2)

// module Plot 
// [<Sealed>]
// type plot =
//     static member ( + )  : plot * plot -> plot


// module Plot 
//     type Plot =  
//         | Opr of opr
//         | Seq of plot * plot
//         static member ( + ) (Seq(Seq(x1),Seq(x2)),Seq(Seq(y1),Seq(y2))) = Seq(x1 + x2, y1+y2)

