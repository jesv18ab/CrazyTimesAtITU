
type item = {
    id : int;
    name : string;
    price : float
    };;


type register = item list;;


(*
Question 1.1 - making a value for a register type variable
*)
let basket = [
    {id = 1; name = "Milk"; price= 8.75};
    {id = 2; name = "Juice"; price= 16.25};
    {id = 3; name = "Rye Bread"; price= 25.00};
    {id = 4; name = "White Bread"; price= 18.50}
];;

exception Register of string;;



(*
Helper method to assignment 1.2
Collects all ids from the item list
*)
let collectIds r =r|>List.collect(fun g -> g.id::[]);;

(*
Solution to 1.2
* collects all ids
* checks if an id exists that matches the int argument 
* if false, then throw an exception 
* else find the particular instance matching by id
* IMPORTANT: Does not match the type form the description 
* neither does the suggested solution, so think it is fine
*)
let getItemById i r =
 if( collectIds(r) |> List.contains i = false)
 then raise (Register "Index out of bounce")
 else r |> List.filter(fun { id = id; name=_;price=_ } -> id = i);;


(*
1.3 - nextID
* find the next available id in the basket list. 
* We assumes that there are no cases wherein we have that ids: 1, 2, 4 and 5 are used 
* We just finds the biggest id and increases this by one.  
*)


let nextId r =
    if(List.isEmpty r = true) then 1
    else collectIds(r) |> (List.maxBy id) |> ( fun x -> x+1);; 
//printfn "%A" (nextId basket);;


(*
1.4 - addItem
Adds item to list. Little unnecessary detail
But in order for the item to be added in the end of the list
I make the element as a list and concatenates both lists to onw list
Suggested solution simply adds the item - could this give issues with regard to type?
*)
let addItem n p r = r @ [{ id = nextId r; name= n; price = p}] ;;

(*
let addItem n p r = {id=nextId01 r; name=n; price=p} :: r
let ut05 = addItem "Water" 10.25 v = {id=5;name="Water";price=10.25} :: v
*)

(*
1.5 - deleteItemById
* Removes an element by filtering for all ids that are equal to the id
* passed to the function
*)


let deleteItemById i r = r|> List.filter(fun {id = id; name = _ ; price = _} -> id <> i );;


(*
1.6 - uniqueRegister
* collects all ids in list.
* Counts the amount of occurence for each id and stores it as a key-value tuple
* maxBy snd means, the max value for each entry, when comparing values
* maxBy can be used with, id, fst(first) and snd(second argument.)
* In this case fst would be equal to id, as id is the first argument in the tuple. 
*Finally, we use the max value as argument to check whether or not we have duplicates
*)


let basketTwo = [
    {id = 1; name = "Milk"; price= 8.75};
    {id = 2; name = "Juice"; price= 16.25};
    {id = 2; name = "Juice"; price= 16.25};
    {id = 3; name = "Rye Bread"; price= 25.00};
    {id = 4; name = "White Bread"; price= 18.50}
];;


let uniqueRegister r = r|> collectIds |> List.countBy id |> List.maxBy snd |> (fun (_, v) -> if v = 1 then true else false ) ;;  
//printfn "%A" (uniqueRegister basketTwo);;
//printfn "%A" (uniqueRegister basket);;

(*
1.7 - itemsInPriceRange
* Uses the filter and match the price for each entry against the price range
* price range is: 
* (p-d) <= price && price <= (p+d)
*)


let itemsInPriceRange p d r = r |> List.filter(fun {id = _; name = _; price = price} -> (p-d) <= price && price <= (p+d));;

//printfn "%A" (itemsInPriceRange 17.0 2.0 basket );;

(*
2.1 - declaration
This recursive function computes the repeated 
multiplication from N til N + m;
*)

let rec f n m =
if m=0 then n
else n * f (n+1) (m-1);;


(*
2.2 - Tail recursion and continuation 
tail recursion: 
A inner recuresive function is the last part of the original function, f'.
For each recrusive call, we multiply the value of acc with n that is incremented for each recursion. 


continuation: 
Similar to tail recursion. However, this approach passes af function into the recursive call as the last argument 
Here the function takes the values of n compute the product. 

*)

let f' n m =
   let rec inner n m acc =
    match (n,m) with
    |(n, 0 ) -> acc * n
    |(n,m) ->  inner (n+1) (m-1) (n*acc) ;
   inner n m 1;;
   
   
let fContinuation n m =
   let rec inner n m c =
    match (n,m) with
    |(n, 0 ) -> c n
    |(n,m) ->  inner (n+1) (m-1) (fun x -> c(n*x))
   inner n m id;;   
   
  //printfn "%A" (fContinuation 10 3);;   


(*
2.3
type: 'a list -> 'a list -> ('a*'a) list 

This method takes two list arguments and produces a list containing a list of 2-tuples. 
We have three cases: 
1: If ys is empty, then we will create a 2-tuple of the current head from the xs list. 
2: If xs is empty, we will create a 2-tuple using the current head of the ys list
3: If non of the tuples are empty, we will create a 2-tuple using the heads from each list. 
*)  
let rec z xs ys =
 match (xs, ys) with
([],[]) -> []
 | (x::xs,[]) -> (x,x) :: (z xs ys)
 | ([],y::ys) -> (y,y) :: (z xs ys)
 | (x::xs,y::ys) -> (x,y)::(z xs ys);;

(*
2.4 
type: 'a list -> 'a list -> 'a list  
1: Either list, ys is empty, which means we just append whatever is remaining in xs to the list that has been so far created during the recursive call
2: same as above, but just with the ys list.
3: We have some elements in bot lists, in which case we cons the head of each list with the list we are creating recursively, starting with the head from xs.

*)

let rec s xs ys =
 match (xs,ys) with
 ([],[]) -> []
| (xs,[]) -> xs
| ([],ys) -> ys
| (x::xs,y::ys) -> x::y::s xs ys;;

//case one
let l1 = [1..3];;
let l2 = [4..8];;
//printfn "%A" (s l1 l2);;


// case two
let l3 = [1..10];;
let l4 = [4..6];;
//printfn "%A" (s l3 l4);;

//case three, actually also covered in the fist two cases

let l5 = [1..10];;
let l6 = [10..20];;
//printfn "%A" (s l5 l6);;
    

(*
2.5
The first version is continuation(I like this better ;) ) 
Here we pass a function that takes the two arguments from the pattern matching and cons them into a ist.
If no more elements are left in ys, we cons the rest of the element from the xs list. Same goes for ys, if xs is empty
If both lists are empty, we forward the empty list as argument to the continuation function

The second example is classic tail recursion.
Same principle, but just with a accumulating list as the last argument to the inner function. 
*)

let s' xs ys =
   let rec inner xs ys c =
    match (xs,ys) with
    |([],[]) -> c []
    | (xs,[]) -> c xs
    | ([],ys) -> c ys
    | (x::xs,y::ys) -> inner xs ys (fun listToPopulate -> c(x::y::listToPopulate))
   inner xs ys id;;
   
let sTail xs ys =
   let rec inner xs ys acc =
    match (xs,ys) with
    |([],[]) -> []
    | (xs,[]) -> acc@xs
    | ([],ys) -> acc@ys
    | (x::xs,y::ys) -> inner (xs) (ys) (x::y::acc)
   inner xs ys [];;
      
   (*
printfn "%A" (s l1 l2)   
printfn "%A" (s' l1 l2)   
printfn "%A" (sTail l1 l2)   
   *)
 
 
 //Q. 3
 
type Latex<'a> =
    Section of string * 'a * Latex<'a>
  | Subsection of string * 'a * Latex<'a>
  | Label of string * Latex<'a>
  | Text of string * Latex<'a>
  | Ref of string * Latex<'a>
  | End

//3.1 
//Type - Latex<'a option>
let text1 = Section ("Introduction", None,
 Text ("This is an introduction to ...",
 Subsection ("A subsection", None,
 Text ("As laid out in the introduction we ...",
 End))));;

 
 //3.2

//I think this solution is retarded
(*
let addSecNumbers l = 
  let rec addSecNumbers' secNum subNum = function
  | (Section(intro, _, latex)) -> Section(intro, (secNum+1).ToString(), addSecNumbers' (secNum+1) 0 latex)
  | Subsection(subText, _, latex) -> Subsection( subText, secNum.ToString() + "." + (subNum+1).ToString(), addSecNumbers' (secNum) (subNum+1) latex )
  | Text(text, latex) -> Text(text, addSecNumbers' secNum subNum latex)
  | End-> End
  addSecNumbers' 0 0 l;;
 
 *)
 
//3.3
// type - Latex<'a> -> Latex<string>

// 3.4

let text3 = Section ("Introduction", "1",
    Label("intro.sec",
    Text ("In section",
    Ref ("subsec.sec",
    Text (" we describe ...",
    Subsection ("A subsection", "1.1",
    Label("subsec.sec",
    Text ("As laid out in the introduction, Section ",
    Ref ("intro.sec",
    Text (" we ...",
    End))))))))));;
 
 
 (*let buildLabelEnvTwo t =
  let env = Map.empty
  let rec buildLabelEnv' sec env = function
    | Section(_,sec,rest) -> buildLabelEnv' sec env rest
    | Subsection(_,sec,rest) -> buildLabelEnv' sec env rest
    | Label(lab,rest) -> buildLabelEnv' sec (Map.add lab sec env) rest
    | Text(_,rest) -> buildLabelEnv' sec env rest
    | Ref(_,rest) ->  buildLabelEnv' sec env rest
    | End -> env
  buildLabelEnv' "" env text3;;
 
printf "%A" (buildLabelEnvTwo text3 );;
*)

(*
The code produces a sequence of integers. 
The content of this sequence consists of the negated values of all integers that simplifies to 0, when computing their module to 2
All integers that does not simplify to 0, will be stores as their normal value
*)
let mySeq = Seq.initInfinite (fun i -> if i % 2 = 0 then -i else i);;
//printfn "%A" (Seq.take 10 mySeq);;

let infiniteSeq =  Seq.initInfinite(id);;
let seqNumbers m = Seq.take (m*2) infiniteSeq;;
let finSeq n m = seqNumbers (m)|> Seq.map( fun i -> i*2 + n);;


(*
sX type: X list -> X list -> X list
uzX type: X list -> X list * X list

sX: This method takes two List arguments of type X.
these listed are matched in three cases:
1: First we try to get the A and B values form each list and store them in a List of X type C, containing Int tuples (int * int)
2: If both list are empty, we terminate the function 
3: If bot lists do not containt an equal amount of values consistently being A and B, we return failwith with an erro message. 

input
let AList = [
    A 2; A 4
];;
let BList = [
    B 2;
    B 2;
];;
output = [C(2,2); C(2,4)]

input
let AList = [
    A 2; A 4
];;
let BList = [
    A 2;
];;

output = Error

uzX: Takes an X list containing C elements and computes a two X lists
First list is an A list of a ll the first elements from the C type tuples
Second list is the same just with B values.

If the arguemnt forwarded he method is not a list containing C tuples, the function will terminate with an error

input
let AList = [
    C (2,4); C (4,4)
];;

output:
([A 2; A 4], [B 4; B 4])


input
let AList = [
    A (2); B (4)
];;

output:
Error

*)

type X =
    |A of int
    |B of int
    |C of int * int
let rec zX xs ys =
    match (xs,ys) with
    (A a::aS,B b::bS) -> C(a,b) :: zX aS bS
    | ([],[]) -> []
    | _ -> failwith "Error";;
  
let rec uzX xs =
    match xs with
    C(a,b)::cS ->
        let (aS,bS) = uzX cS
        (A a::aS,B b::bS)
        | [] -> ([],[])
        | _ -> failwith "Error";;

printfn "%A" (finSeq 2 10);;