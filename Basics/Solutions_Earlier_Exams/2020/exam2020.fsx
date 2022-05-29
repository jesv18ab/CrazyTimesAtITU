(*
    I hereby declare that I myself have created this exam hand–in in 
    its entirety without help from anybody else.

    Andreas Højgaard Jørgensen @ ITU 
*)

// Question 1 

type mymap<'a,'b> = MyMap of list<'a *'b>

let ex1 = MyMap [('A',65);('B',66);('C',67)]

// Question 1.1 

let dice1 = MyMap [(1,4);(2,2);(3,3);(4,2);(5,2);(6,2)]

let dice2 = MyMap [(1,4);(2,2);(3,3);(4,3);(5,5);(6,3)]

// Ex1 is of type: mymap<char,int> 
// Dice1 is of type: mymap<int,int>

let emptyMap () = MyMap ([])

let size (MyMap list) = list.Length
size ex1
size (emptyMap())

// Question 1.2 

let isEmpty (MyMap list) = if list.IsEmpty then true else false

isEmpty ex1
isEmpty (emptyMap())

// Question 1.3 

let tryFind k (MyMap m) = 
    match (Map.tryFind (k) (Map.ofList m) ) with //Convert to map and try find key
    | Some value -> Some (k,value) //return value is option type  
    | None -> None

tryFind 'B' ex1
tryFind 'D' ex1 

// since 'a get placed into a map, it becomes the key. 
// it's required to have comparison on the key value, as they need to be distinct from each other in a map. 

let remove k (MyMap m) = 
    match (tryFind k (MyMap m)) with 
    | Some (key, value) -> Map.remove key (Map.ofList m) |> Map.toList |> (fun x -> (MyMap x)) //Convert to map, back to list and finally back to MyMap. 
    | None -> (MyMap m)

remove 'B' ex1

let add k v (MyMap m) = Map.add k v (Map.ofList m)|> Map.toList |> (fun x -> (MyMap x))

add 'D' 68 ex1
add 'A' 222 ex1

// Question 1.3

let upd f k v (MyMap m) =
    match ((tryFind k (MyMap m))) with 
    | Some (key, value) -> Map.add key (f value v) (Map.ofList m) |> Map.toList |> (fun x -> (MyMap x)) // Apply function f 
    | None -> add k v (MyMap m)

upd(+)'A' 65 ex1
upd(+)'D' 68 ex1

let map f (MyMap m) = List.map f m |> (fun x -> (MyMap x))

map (fun (k,v) -> (k,(v + 2))) ex1 // I modified the example function, to take in a tuple and return a tuple instead.

let fold f s (MyMap m) = List.fold f s m 
fold (fun s (k,v) -> s+v) 0 dice1 // again, I modified the f input to be a tuple instead of seperate arguments. 


// Question 2 

let even n = if n % 2 = 0 then true else false

even 42

let collatz n = 
    match (even n) with 
    | true -> n/2
    | false -> 3*n+1


let collatz' n = if n <= 0 then failwith "collatz’: n is zero or less."  else collatz n

// collatz' 0


// Question 2.2 

let rec help f n i = 
    match i with 
    | 0 -> n
    | _ -> help f (f n) (i-1)

let applyN f n N = List.init (N+1) (fun i -> help f n i)

applyN collatz 42 8

collatz 0

let rec help2 f n count = 
    match n with 
    | 1 -> count
    | _ -> help2 f (f n) (count + 1)

let applyUntilOne f n = help2 f n 0

applyUntilOne collatz 42 

// Question 2.3 

let rec mySeq f x = seq { yield x 
                          yield! mySeq f (f x)}


mySeq collatz 42

(*
    For each recursive iteration of mySeq x is given as output. 
    Aftwards x is applied to collatz and recursively called by MySeq.

    This produces a infinite seq where collatz is applied to every result of the last iteration. 
*)

let g x = x * 2

mySeq g 1

// Question 3 

type name = string
type quantity = float
type date = int * int * int
type price = float
type transType = Buy | Sell
type transData = date * quantity * price * transType 
type trans = name * transData

let ts : trans list = [("ISS", ((24,02,2014),100.0,218.99,Buy)); ("Lego",((16,03,2015),250.0,206.72,Buy));
                        ("ISS", ((23,02,2016),825.0,280.23,Buy)); ("Lego",((08,03,2016),370.0,280.23,Buy)); ("ISS", ((24,02,2017),906.0,379.46,Buy)); ("Lego",((09,11,2017), 80.0,360.81,Sell)); ("ISS", ((09,11,2017),146.0,360.81,Sell)); ("Lego",((14,11,2017),140.0,376.55,Sell)); ("Lego",((20,02,2018),800.0,402.99,Buy)); ("Lego",((02,05,2018),222.0,451.80,Sell)); ("ISS", ((22,05,2018),400.0,493.60,Buy)); ("ISS", ((19,09,2018),550.0,564.00,Buy)); ("Lego",((27,03,2019),325.0,625.00,Sell)); ("ISS", ((25,11,2019),200.0,680.50,Sell)); ("Lego",((18,02,2020),300.0,720.00,Sell))]

// Question 3.1 
let addTransToMap (name, newData) (m:Map<name,transData list>) =
    match (Map.tryFind name m) with 
    | Some data -> Map.add name ([newData]@data) m
    | None -> Map.add name [newData] m

let m1 = addTransToMap ("ISS", ((24,02,2014),100.0,218.99,Buy)) Map.empty
let m2 = addTransToMap ("ISS", ((22,05,2018),400.0,493.60,Buy)) m1

let shares = List.foldBack (fun trans acc -> addTransToMap trans acc ) ts Map.empty

let accTrans (tq:float,avg:float) ((d,q,p,tType):transData) = 
    match tType with
    |Sell -> (tq-q,avg)
    |Buy -> (tq+q,(avg*tq+q*p)/(tq+q))

let quantityAndAvgPrice ts = List.fold accTrans (0.0,0.0) ts

quantityAndAvgPrice [((24,02,2014),100.0,218.99,Buy); ((23,02,2016),825.0,280.23,Buy)]

let res = Map.map (fun key data -> quantityAndAvgPrice data) shares

// Question 4

let rec dup = function 
              [] -> []
              | x::xs -> x::x::dup xs

dup [0;1]

(*
    Given an input list [0;1], the the returned list is [0; 0; 1; 1]
    The function produce a new list with duplicated elements from the last list. 
*)

let rec dupA acc list = 
    match list with 
    | [] -> acc
    | x::xs -> dupA (acc @ [x] @ [x]) xs

dupA [] [0;1]

// Question 4.2 

let replicate2 i = seq {yield i; yield i}

let replicate22 i = Seq.append (Seq.singleton i) (Seq.singleton i)

let replicate222 i = Seq.ofList [i;i]

replicate2 4

let infi = Seq.initInfinite id 
let dupSeq = seq {for i in infi do yield! replicate2 i }

// Question 4.3 

let dupSeq2 s = seq { for i in s do yield! replicate2 i}

dupSeq2 (seq[1;2]) 