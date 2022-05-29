(*

Problem 1 (25%)

In this problem we will consider a simple cash register, where items are 
identified by a unique id. An item also has a name and a unit price.

This leads to the following declarations:
*)

type item = {id : int;
             name : string;
             price : float}

type register = item list

(* 1.1 Declare a value of type register with the following four items:
     a) Item with id 1 named "Milk" with price 8.75
     b) Item with id 2 named "Juice" with price 16.25
     c) Item with id 3 named "Rye Bread" with price 25.00
     d) Item with id 4 named "White Bread" with price 18.50
*)

let v = [{id=1; name="Milk";        price= 8.75};
         {id=2; name="Juice";       price=16.25};
         {id=3; name="Rye Bread";   price=25.00};
         {id=4; name="White Bread"; price=18.50}]

(* 1.2 Declare an exception Register and a function 
getItemById: int -> register -> item to
extract the first occurrence of an item with the specified id from the
register. The function should raise the exception Register if the item
is not in the register. The exception should contain an additional
string explaining the error.
*)

exception Register of string

let getItemById01 id r =
  match List.filter (fun {id=id';name=_;price=_} -> id=id') r with
    [] ->  raise (Register ("Item with id " + id.ToString() + " does not exists."))
  | x::xs -> x

let ut01 = List.map (fun id -> getItemById01 id v) [1;2;3;4] = v

let rec getItemById02 id = function
  [] -> raise (Register ("Item with id " + id.ToString() + " does not exists."))
| ({id=id';name=_;price=_} as item) :: rs -> if id = id' then item else getItemById02 id rs

let ut02 = List.map (fun id -> getItemById02 id v) [1;2;3;4] = v

(* 1.3: Declare a function nextId r : register -> int that returns the
next id to use in the register. If maxId is the maximum id currently
used in register r, then the next id is defined as maxId + 1. The
first id to use on an empty register is 1.
*)

let nextId01 r = (List.fold (fun maxId {id=id;name=_;price=_} -> if maxId < id then id else maxId) 1 r) + 1
let nextId01' r = (List.fold (fun maxId {id=id;name=_;price=_} -> if maxId < id then id+1 else maxId) 1 r)
let nextId01'' r = (List.fold (fun maxId {id=id;name=_;price=_} -> max id maxId) 1 r) + 1

let ut03 = nextId01 v = 5
let ut03' = nextId01' v = 5
let ut03'' = nextId01'' v = 5

let rec nextId02 = function
  [] -> 1
| {id=id;name=_;price=_} :: rs -> let maxId = nextId02 rs
                                  if maxId < id then id+1 else maxId
let ut04 = nextId02 v = 5

(* 1.4: Declare a function 
  addItem n p r: string -> float -> register -> register 
that adds a new item with name n and unit price p to the
register r. The new item must have the next available id as defined by
the function nextId. *)
let addItem n p r = {id=nextId01 r; name=n; price=p} :: r
let ut05 = addItem "Water" 10.25 v = {id=5;name="Water";price=10.25} :: v

(* 1.5: Declare a function 
  deleteItemById id r: int -> register -> register 
that deletes an item with id id and returns the updated
register. The register is unchanged if no item with id id exists. *)
let deleteItem01 id r = List.filter (fun {id=id';name=_;price=_} -> id' <> id) r
let ut06 = deleteItem01 1 v = List.tail v

let rec deleteItem02 id = function
  [] -> []
| ({id=id';name=_;price=_} as x)::rs -> let rest = deleteItem02 id rs
                                        if id=id' then rest else x::rest
let ut07 = deleteItem02 1 v = List.tail v

(* 1.6: Declare a function 
  uniqueRegister r : register -> bool
to return true if all items in the register have unique id's and
otherwise false. *)
let uniqueRegister01 r = let ids = List.map (fun {id=id;name=_;price=_} -> id) r
                         List.length ids = List.length (Set.toList (Set.ofList ids))  (* replace with Set size *)
let uniqueRegister01' r = let ids = List.map (fun {id=id;name=_;price=_} -> id) r
                          List.length ids = Set.count (Set.ofList ids)
let ut08 = uniqueRegister01 v = true
let ut09 = uniqueRegister01 ({id=2;name="Coca Cola";price=10.25}::v) = false
let ut08' = uniqueRegister01' v = true
let ut09' = uniqueRegister01' ({id=2;name="Coca Cola";price=10.25}::v) = false


let rec uniqueRegister02 = function
  [] -> true
| {id=id;name=_;price=_}::rs -> (not (List.exists (fun {id=id';name=_;price=_} -> id=id') rs)) && (uniqueRegister02 rs)
let ut10 = uniqueRegister02 v = true
let ut11 = uniqueRegister02 ({id=2;name="Coca Cola";price=10.25}::v) = false

(* 1.7: Declare a function 
  itemsInPriceRange p d r: float -> float -> register -> register
Given a price p and a delta d, the function must
return all items in the register r with a price within the range 
  [p-d,..., p+d]. 
The function returns a new register. You may assume delta d
to be positive. *)
let itemsInPriceRange01 p d r =
  let (minP,maxP) = (p-d,p+d)
  List.fold (fun r' ({id=_;price=p';name=_} as x) -> if p' >= minP && p' <= maxP then x::r' else r') [] r

let ut12 = itemsInPriceRange01 16.0 8.0 v = [{id=4; name="White Bread"; price=18.50};
                                             {id=2; name="Juice";       price=16.25};
                                             {id=1; name="Milk";        price= 8.75}]

let itemsInPriceRange02 p d r =
  List.fold (fun r' ({id=_;price=p';name=_} as x) -> if abs(p-p') <= d then x::r' else r') [] r

let ut12' = itemsInPriceRange02 16.0 8.0 v = [{id=4; name="White Bread"; price=18.50};
                                              {id=2; name="Juice";       price=16.25};
                                              {id=1; name="Milk";        price= 8.75}]
(* Or use List.filter ... *)

let rec itemsInPriceRange03 p d = function
  [] -> []
| {id=_;price=p';name=_} as x::rs -> let r' = itemsInPriceRange03 p d rs
                                     if abs(p-p') <= d then x::r' else r'

let ut13 = itemsInPriceRange03 16.0 8.0 v = [{id=1; name="Milk";        price= 8.75};
                                             {id=2; name="Juice";       price=16.25};
                                             {id=4; name="White Bread"; price=18.50}]


(* Problem 2 (25%) *)

(* 2.1: Consider the following F# declaration: *)
let rec f n m =
  if m=0 then n
  else n * f (n+1) (m-1)
let ut33 = List.map (f 10) [0;1;2;3] = [10;10*11;10*11*12;10*11*12*13]
(* Give the type of f and describe what f computes. Your description
should focus on what f computes, rather than on individual computation
steps.

Type of f: int -> int -> int

Description: f computes [(n+0)*(n+1)*...*(n+m)], for m>=0
             f is an infinite loop for m<0 
*)

(* 2.2: The function f is not tail recursive. Declare a function f'
that is tail recursive. Hint: You can either use a continuation function or
an accumulating parameter. *)

let fA n m =
  let rec fA' n m acc =
    if m=0 then n*acc
    else fA' (n+1) (m-1) (n*acc)
  fA' n m 1

let ut34 = List.map (fA 10) [0;1;2;3] = [10;10*11;10*11*12;10*11*12*13]

let fC n m =
  let rec fC' n m c =
    if m=0 then c n
    else fC' (n+1) (m-1) (fun acc -> c (n*acc))
  fC' n m id
let ut35 = List.map (fC 10) [0;1;2;3] = [10;10*11;10*11*12;10*11*12*13]

(* 2.3: Consider the following F# declaration: *)
let rec z xs ys =
  match (xs, ys) with
    ([],[]) -> []
  | (x::xs,[]) -> (x,x) :: (z xs ys)
  | ([],y::ys) -> (y,y) :: (z xs ys)
  | (x::xs,y::ys) -> (x,y)::(z xs ys)

let ut21 = z [1;2;3] [10;11;12] = [(1,10);(2,11);(3,12)]
let ut22 = z [1;2] [10;11;12] = [(1,10);(2,11);(12,12)]
let ut23 = z [1;2] [10] = [(1,10);(2,2)]
let ut24 = z [] [] = []

(* Give the type of z and describe what z computes. Your description
should focus on what z computes, rather than on individual computation
steps. Give two examples of input and result that support your
description.

Type of z: 'a list -> 'a list -> ('a*'a) list

Description: The function z is a special version of zip where the two
lists xs and ys are zipped together. If xs and ys are not of the same
length, then the last elements in the longest list are used as both
elements in the pair elements in the result list.

Examples:
  z [1;2;3] [10;11;12] = [(1,10);(2,11);(3,12)]
  z [1;2] [10;11;12] = [(1,10);(2,11);(12,12)]
*)

(* 2.4: Consider the following F# declaration: *)
let rec s xs ys =
  match (xs,ys) with
    ([],[]) -> []
  | (xs,[]) -> xs
  | ([],ys) -> ys
  | (x::xs,y::ys) -> x::y::s xs ys

let ut25 = s [1;2;3] [10;11;12] = [1;10;2;11;3;12]
let ut26 = s [1;2] [10;11;12;13] = [1;10;2;11;12;13]
let ut27 = s [1;2] [10] = [1;10;2]
let ut28 = s [] [] = []

(* Give the type of s and describe what s computes. Your description
should focus on what s computes, rather than on individual computation
steps. Give two examples of input and result that support your
description.

Type of s: 'a list -> 'a list -> 'a list

Description: The function s is a special version of splice where the
two lists xs and ys are spliced together. If xs and ys are not of the
same length, then the last elements in the longest list are used as
tail in the result list.

Examples:
  s [1;2;3] [10;11;12] = [1;10;2;11;3;12]
  s [1;2] [10;11;12;13] = [1;10;2;11;12;13]
*)

(* 2.5: The function s above is not tail recursive. Declare a function
sC being a tail recursive version of the function s. Hint: You can use 
a continuation function. *)

let sC xs ys =
  let rec sC' xs ys c =
    match (xs,ys) with
      ([],[]) -> c []
    | (xs,[]) -> c xs
    | ([],ys) -> c ys
    | (x::xs,y::ys) -> sC' xs ys (fun zs -> c (x::y::zs))
  sC' xs ys id

let ut29 = sC [1;2;3] [10;11;12] = [1;10;2;11;3;12]
let ut30 = sC [1;2] [10;11;12;13] = [1;10;2;11;12;13]
let ut31 = sC [1;2] [10] = [1;10;2]
let ut32 = sC [] [] = []  



(* Problem 3 (30%) *)

(* Consider the following F# declarations *)

type Latex<'a> =
    Section of string * 'a * Latex<'a>
  | Subsection of string * 'a * Latex<'a>
  | Text of string * Latex<'a>
  | End

let text1 = Section ("Introduction", None,
              Text ("This is an introduction to ...",
                Subsection ("A subsection", None,
                  Text ("As laid out in the introduction we ...",
                    End))))

(* 3.1: What is the type of the declaration text1 above? *)
(* Type: Latex<'a option> *)

(* The type Latex<'a> represents simple LaTeX-alike documents and the
   value above represents the following text

   1 Introduction
   This is an introduction to ...
   1.1 A subsection
   As laid out in the introduction we ...
*)

(* 3.2: Declare a function 
  addSecNumbers t 
that transforms a value as
text1 above into a new value with section numbers added. For instance

  addSecNumbers text1

must return the following value

  Section ("Introduction", "1",
    Text ("This is an introduction to ...",
      Subsection ("A subsection", "1.1",
        Text ("As laid out in the introduction we ...",
          End))))

For a more complicated example consider the F# declaration
*)

let text2 = Section ("Introduction", None,
              Text ("This is an introduction to ...",
                Subsection ("A subsection", None,
                  Text ("As laid out in the introduction we ...",
                    Subsection ("Yet a subsection", None,
                      Section ("And yet a section", None,
                        Subsection ("A subsection more...", None,
                          End)))))))

(* The declaration text2 represents the following text

  1 Introduction
  This is an introduction to ...
  1.1 A subsection
  As laid out in the introduction we ...
  1.2 Yet a subsection
  2 And yet a section
  2.1 A subsection more...

The function application

  addSecNumbers text2

must return the value
*)

let text2' = Section ("Introduction", "1",
               Text ("This is an introduction to ...",
                 Subsection ("A subsection", "1.1",
                   Text ("As laid out in the introduction we ...",
                     Subsection ("Yet a subsection", "1.2",
                       Section ("And yet a section", "2",
                         Subsection ("A subsection more...", "2.1",
                           End)))))))
                           
let addSecNumbers t =
  let rec addSecNumbers' sec subSec = function
      Section(text,_,rest) -> Section(text,(sec+1).ToString(), addSecNumbers' (sec+1) 0 rest)
    | Subsection(text,_,rest) -> Subsection(text,sec.ToString() + "." + (subSec+1).ToString(), addSecNumbers' sec (subSec+1) rest)
    | Text(text,rest) -> Text(text,addSecNumbers' sec subSec rest)
    | End -> End
  addSecNumbers' 0 0 t

let ut100 = addSecNumbers text2 = text2'

(* 3.3: What is the type of the function addSecNumbers *)
(* Type: Latex<'a> -> Latex<string> *)

(* We now extend the type Latex to also include labels and references. *)

type Latex<'a> =
    Section of string * 'a * Latex<'a>
  | Subsection of string * 'a * Latex<'a>
  | Label of string * Latex<'a>
  | Text of string * Latex<'a>
  | Ref of string * Latex<'a>
  | End

(* Consider the following F# declaration *)
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
                                End))))))))))

(* 3.4: Declare a function buildLabelEnv t: LaTeX<'a> -> Map<string,string> that given a declaration such as
text3 above returns an environment that maps label names to sections. You
can assume that the function addSecNumbers has been implemented on the
extended version of the type Latex<'a>.

The type of the environment is Map<string, string>. For the example text3, the returned environment contains
the following two entries:
  "intro.sec" -> "1"
  "subsec.sec" -> "1.1"

Hint: First you call addSecNumbers t and thereby are sure you have section numbers defined.
*)

let addSecNumbers t =
  let rec addSecNumbers' sec subSec = function
      Section(text,_,rest) -> Section(text,(sec+1).ToString(), addSecNumbers' (sec+1) 0 rest)
    | Subsection(text,_,rest) -> Subsection(text,sec.ToString() + "." + (subSec+1).ToString(), addSecNumbers' sec (subSec+1) rest)
    | Label(lab,rest) -> Label(lab,addSecNumbers' sec subSec rest)
    | Text(text,rest) -> Text(text,addSecNumbers' sec subSec rest)
    | Ref(lab,rest) -> Ref(lab,addSecNumbers' sec subSec rest)
    | End -> End
  addSecNumbers' 0 0 t

let buildLabelEnv t =
  let t' = addSecNumbers t
  let env = Map.empty
  let rec buildLabelEnv' sec env = function
      Section(_,sec,rest) -> buildLabelEnv' sec env rest
    | Subsection(_,sec,rest) -> buildLabelEnv' sec env rest
    | Label(lab,rest) -> buildLabelEnv' sec (Map.add lab sec env) rest
    | Text(_,rest) -> buildLabelEnv' sec env rest
    | Ref(_,rest) ->  buildLabelEnv' sec env rest
    | End -> env
  buildLabelEnv' "" env t'

let ut54 = addSecNumbers text3

let ut58 = buildLabelEnv text3


(* 3.5: Declare a function 
  toString t : Latex<'a> -> string 
  
that makes a string representation of the text as illustrated by the
examples above.

You can use the value nl defined as

  let nl : string = System.Environment.NewLine

to represent a newline.

Hint: Your function toString should make use of the functions addSecNumbers and buildLabelEnv. That is, 
first call addSecNumbers and then buildLabelEnv to build the 
mapping of labels to sections. Then you have the necessary information to make toString.
*)
let nl : string = System.Environment.NewLine

let toString t = 
  let t' = addSecNumbers t
  let env = buildLabelEnv t'
  let rec toString' = function
    Section(text,sec,rest) -> nl + sec + " "  + text + nl + toString' rest
  | Subsection(text,sec,rest) -> nl + sec + " " + text + nl + toString' rest
  | Label(lab,rest) -> toString' rest
  | Text(text,rest) -> text  + toString' rest
  | Ref(lab,rest) ->  " " + (Map.find lab env) + " " + toString' rest
  | End -> nl
  toString' t'

let ut83 = toString text3

(* Problem 4 20%*)

(* 4.1: Consider the F# declaration. Write the result type and result value of evaluating
Seq.take 10 mySeq.
Type: seq<int>  *)
let mySeq = Seq.initInfinite (fun i -> if i % 2 = 0 then -i else i)
let ut43 = Seq.toList <| Seq.take 10 mySeq = [0;1;-2;3;-4;5;-6;7;-8;9]

(* 4.2: Declare the function finSeq n M : int -> int -> seq<int> that 
   defines the finite sequence n+2m for m \in [0,...,M] *)
let finSeq01 n M = Seq.map (fun m -> n+2*m) [0..M]
let ut50 = List.map (fun i -> Seq.toList <| finSeq01 10 i) [1;2;3] = [[10; 12]; [10; 12; 14]; [10; 12; 14; 16]]

let finSeq02 n M = seq {for m in [0..M] do
                          yield n+2*m}
let ut51 = List.map (finSeq02 10) [1;2;3] = [seq [10; 12]; seq [10; 12; 14]; seq [10; 12; 14; 16]]

let finSeq03 n M = Seq.init (M+1) (fun m -> n+2*m)
let ut52 = List.map (fun i -> Seq.toList <| finSeq03 10 i) [1;2;3] = [[10; 12]; [10; 12; 14]; [10; 12; 14; 16]]

let finSeq04 n M = seq [n .. 2 .. n+2*M]
let ut53 = List.map (fun i -> Seq.toList <| finSeq04 10 i) [1;2;3] = [[10; 12]; [10; 12; 14]; [10; 12; 14; 16]]

(* 4.3: Consider the following F# declarations *)
type X = A of int | B of int | C of int * int

let rec zX xs ys =
  match (xs,ys) with
    (A a::aS,B b::bS) -> C(a,b) :: zX aS bS
  | ([],[]) -> []
  | _ -> failwith "Error"

let rec uzX xs =
  match xs with
    C(a,b)::cS -> let (aS,bS) = uzX cS
                  (A a::aS,B b::bS)
  | [] -> ([],[])
  | _ -> failwith "Error"  

(* Give the types of the functions zX and uzX. Describe what the two
functions compute. Your description should focus on what is computed,
rather than the individual computation steps. Give, for each function,
three example input values and result values that support your
descriptions. *)

(*
Type of zX: X list -> X list -> X list
Type of uzX: X list -> X list * X list

Description of zX: The function zX zips two lists of elements of type
X. It is required that all elements in the first list are values of the
constructor A and all elements in the second list are values of the
constructor B. It is also required that the two lists have the same
number of elements. For all other input data the function fails.

Example data zX:
  zX [A 2; A 3] [B 3; B 8] = [C(2,3);C(3,8)]
  zX [A 2; B 3] [B 4; C (23,21)] fails
  zX [A 2; A 3] [B 4; B 8; B 10] fails
  
Description of uzX: The function uzX unzip a list of values of the 
constructor C(v1,v2). For each constructed value, the first 
element v1 is returned in the first list constructed with A and 
the second element v2 is returned in the second list constructed 
with B. For all input lists, containing values constructed with 
either A or B, the function fails.

Example data uzX:
  uzX [C (1,2); C(42,23)] = ([A 1; A 42], [B 2; B 23])
  uzX [C (1,2); A 42] = fails
  uzX [B 1; B 42] fails
*)

