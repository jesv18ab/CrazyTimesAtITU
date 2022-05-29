

open System

type mymap<'a,'b> = MyMap of list<'a*'b>

let ex1 = MyMap [('A',65);('B',66);('C',67)];;
let ex1' = MyMap [('C',67);('A',65);('B',66)];;

//TYPE: mymap<char,int>
(*
Both of the maps are of type mymap<char,int>, stating that the maps consists of char, int value paris in the manner of tupels
*) 
let dice1 = MyMap [('1',4);('2',2);('3',3);('4',2);('5',2);('6',2)];;
let dice2 = MyMap [('1',4);('2',2);('3',3);('4',3);('5',5);('6',3)];;

let emptyMap() = MyMap ([]);;

let size (MyMap list) = list.Length;;

let isEmpty (MyMap m) = if( m.Length = 0 ) then true else false;;


let tryFind k (MyMap m) = if( m|>Map.ofList|>Map.containsKey k) then m|>Map.ofList|>Map.find k|>(fun i -> Some i) else None;;

let remove k (MyMap m) = m|>Map.ofList|>Map.remove k;;

let checkKey k (MyMap m) = Map.ofList m|>Map.containsKey k
let mapOfList (MyMap m) = m|>Map.ofList;;

let add k v (m) =
                if( checkKey k m ) then
                    let removed = remove k m
                    let add = removed|>Map.add k v
                    add
                else mapOfList m|> Map.add k v;;
                
//Better solution
let addBetter k v (MyMap m) = Map.add k v (Map.ofList m)|> Map.toList |> (fun x -> (MyMap x))

let upd f k v (m) =
        if (checkKey k m) then
            let v1 = mapOfList m|>Map.find k
            let newVal = f v v1
            add k newVal m
        else add k v m;;    
let map f (MyMap m) = List.map f m|>(fun x -> (MyMap x));;
let map2 f (MyMap m) = m|>List.map f |> (fun x -> (MyMap x))

let fold f s (MyMap m) = List.fold(fun state (k, v) -> f state k v ) s m;;
    
let even n = if (n % 2 = 0) then true else false;;
let collatz n = if (even n) then n/2 else 3*n + 1;;

let collatz' n = if(n <= 0) then collatz n else failwith "collatz’: n is zero or less";;

let rec applyN f n N =
    match(n,N) with
    |(_, 0) -> []
    |(n,N) ->(f n)::applyN f (f n) (N-1);;
    
    
let applyUntilOne f n =
  let g = n
  let rec inner f n acc =
        match n with
        |(1) -> if even g then acc else acc+1
        |(n) -> inner f (f n) (acc+1)
  inner f n 0;;
  
  
  (*
    For each recursive iteration of mySeq x is given as output. 
    Aftwards x is applied to collatz and recursively called by MySeq.

    This produces a infinite seq where collatz is applied to every result of the last iteration. 
*)
let rec mySeq f x =
    seq { yield x
          yield! mySeq f (f x)};;
    
// Love these inbuilt functions - NOT EVEN PRESENT IN THE SOLUTION 
let g x = Seq.unfold (fun acc -> Some(acc, acc*2))x
//printfn "%A" (g 1)


type name = string
type quantity = float
type date = int * int * int
type price = float
type transType = Buy | Sell
type transData = date * quantity * price * transType
type trans = name * transData;;


let ts : trans list =
[("ISS", ((24,02,2014),100.0,218.99,Buy)); ("Lego",((16,03,2015),250.0,206.72,Buy));
("ISS", ((23,02,2016),825.0,280.23,Buy)); ("Lego",((08,03,2016),370.0,280.23,Buy));
("ISS", ((24,02,2017),906.0,379.46,Buy)); ("Lego",((09,11,2017), 80.0,360.81,Sell));
("ISS", ((09,11,2017),146.0,360.81,Sell)); ("Lego",((14,11,2017),140.0,376.55,Sell));
("Lego",((20,02,2018),800.0,402.99,Buy)); ("Lego",((02,05,2018),222.0,451.80,Sell));
("ISS", ((22,05,2018),400.0,493.60,Buy)); ("ISS", ((19,09,2018),550.0,564.00,Buy));
("Lego",((27,03,2019),325.0,625.00,Sell)); ("ISS", ((25,11,2019),200.0,680.50,Sell));
("Lego",((18,02,2020),300.0,720.00,Sell))];;

 let addTransToMap (label,transactionList) (m:Map<name,transData list>) =
    match Map.tryFind label m with
        |(Some value) -> Map.add label ([transactionList]@ value) m
        | None-> Map.add label [transactionList] m ;;
let m1 = addTransToMap ("ISS", ((24,02,2014),100.0,218.99,Buy)) Map.empty;;
let m2 = addTransToMap ("ISS", ((22,05,2018),400.0,493.60,Buy)) m1;;

let m11 = addTransToMap ("ISS", ((24,02,2014),100.0,218.99,Buy)) Map.empty;;
let m22 = addTransToMap ("ISS", ((22,05,2018),400.0,493.60,Buy)) m1;;

//remeber that foldback is reversed.

(*
So in fold, you would have;
List.fold.( fun accumulator element -> <-Do something->) startingValue inputList)  )

In foldBack we use:
List.foldBack( fun element accumulator -> <-Do something->) inputList StartingValue(For Acc)))
*)
let shares = Map.empty|>List.foldBack(fun element state -> addTransToMap element state)ts ;;
let extract = List.fold( fun state element -> addTransToMap element state) Map.empty ts;;; 


printfn "%A" shares;;

    
//The function has type 'a list -> 'a list
(*
It takes a list as argument and creates a list of doubles size duplicating every entry form the first list and puts it next to the 
entry from the original value
*)

let list = [1..10];;
let rec dup = function
[] -> []
| x::xs -> x::x::dup xs;;

let dupTail l =
        let rec inner l acc =
            match l with
            | [] -> List.rev acc 
            | x::xs ->inner (xs) (x::x::acc)
        inner l [];;      

let replicate2 i = seq {for _ in 1..2 -> i };;
 //Other Soltuiona
 
let replicate22222 i = seq {yield i; yield i}

let replicate22 i = Seq.append (Seq.singleton i) (Seq.singleton i)

let replicate222 i = Seq.ofList [i;i]

let inifiniteValues = Seq.initInfinite(fun i -> i);;
let dupSeq = dup (inifiniteValues|>Seq.take 10|>Seq.toList)|>List.toSeq;;

//Better way
let dupSeq' = Seq.initInfinite(fun i -> [i;i])|>Seq.concat;;

let dupSeq2 s = Seq.map(fun i -> [i;i]) s|>Seq.concat;;

