

open System
open System.Xml.Serialization

type Rope =
 Leaf of string * int
| Node of Rope * int * Rope;;


//Q1

let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)), 20,Node(Leaf("ogr",3),3,Leaf("amming",6)));;

//1.1
let rope2 = Node(Node(Leaf("_and",4),4,Leaf("_very",5)), 9,Node(Leaf("_much",5),5,Leaf("_F#",3)));;
let rope3 = Node(Node(Leaf("example",8),8,Leaf("with",5)),13,Leaf(" 5_nodes",7));;


//What a hack from the nternet
let length r =
    let rec toList r cont =
        match r with
        | Leaf(string, value) -> cont value
        | Node (left,root, right) -> 
            toList left (fun l -> 
                toList right (fun r -> 
                    cont (l + r)))
    toList r id;;
    
let getVal r =
    match(r) with
    |Leaf(s, i) -> Some i
    |Node(rope, i, rope1) -> None;;
    
//Proper solution    
let lengthSecond r  =
    let rec inner r acc =
        match r with
        |Leaf(s, i) -> acc + i
        |Node(left, i, right) -> acc + ((inner left acc ) + (inner right acc))
    inner r 0
    
let flatten r  =
    let rec inner r c =
        match r with
        |Leaf(s, i) ->c s
        |Node(left, i, right) ->
            inner left (fun l -> 
                inner right (fun r -> 
                    c(l + r)))
    inner r id;;
    
let flattenSecond r  =
    let rec inner r acc =
        match r with
        |Leaf(s, i) -> acc + s
        |Node(left, i, right) -> acc + ((inner left acc ) + (inner right acc))
    inner r "";;   



let maxDepth r = 
    let rec height r n = 
        match r with
        |Leaf(s, i) -> n
        |Node(left, i, right ) ->
            let g = height left n+1
            let h = height right n+1
            if g > h then g else h
    height r 1;;
    
let index i r =
    let s = flatten r
    if (i < 0 || i > s.Length) then failwith "Index out of bounce" else s[i] ;;
 
let concat r1 r2 =
    let length = (flattenSecond r1).Length
    Node(r1, length, r2)

let prettyPrint r = r;;


let list01 = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']

type Bucket<'a> = {
    sizeBucket : int;
    elems : List<'a>
};;
type UList<'a> = Bucket<'a> list;;    
//This is a monomorphic type, as we can only store specific types in the tree.
// A polymorphci type would not care, whether we were storing integers, strings, whatever.

let ulist01 = [ { sizeBucket = 4; elems = ['A';'B';'C';'D'] };
    { sizeBucket = 2; elems = ['E';'F'] };
    { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] };
    { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
    { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
    { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ];;



let ulist02 = [ { sizeBucket = 4; elems =  ['G';'H'; 'I'; 'J'; 'K'; 'L'] };
    { sizeBucket = 2; elems = ['E';'F'] };
    { sizeBucket = 6; elems = ['A';'B';'C';'D'] };
    { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
    { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
    { sizeBucket = 4; elems = ['W'; 'X'; 'Y'] } ];;

//I switched placements for two lists in the original variable.
//The valuation comes out false, as the instances now has a differenct ordering of the list elements, which counts under the
//Evaluation of the boolean expression


(*
In the very definition of the Bucket, we state that the size porperty is of type int,
However, the list type is unsepcifiied, allowing for
polymorphism. However, when make a variable of the Bucket type,
we include chars in the lements property. The compiler while recognize this and
determine that this type variable is of type Bucket<char> list
*)

let emptyUL() : UList<'a> list= [];;;

let sizeUL l = l|>List.fold( fun acc {sizeBucket=s; elems = e} -> e.Length + acc  )0;; 

let isempty (ul: Bucket<'a> list) = ul.IsEmpty;;

let existsUL e ul  = ul|>List.fold( fun acc {sizeBucket=s; elems = e} -> e @ acc)[]|>List.contains e
let itemUL ul i  =
    let acc=[]
    let fullList = ul|>List.map( fun {sizeBucket=s; elems = e} -> e @ acc)|>List.concat
    fullList[i];;

let filterUL p ul =
    let fullList = ul|>List.map( fun {sizeBucket=s; elems = e} -> {sizeBucket=s; elems = (e|>List.sort)})
    let filtered = fullList|>List.map( fun {sizeBucket=s; elems = e} -> {sizeBucket=s; elems = (e|>List.filter(fun i -> i < p))})|>List.filter( fun i -> i.elems.Length > 0 )
    filtered;;
let checkSize ul = ul|>List.map(fun i -> i.sizeBucket = i.elems.Length && i.sizeBucket <= 4);;


let chkUL ul = if (isempty ul = false && List.contains false (checkSize ul) <> true) then true else false;;

let map f ul =
    let updated = ul|>List.map(fun {sizeBucket=s; elems = e} -> {sizeBucket=s; elems = e|>List.map(fun i -> f i)})
    updated;;
    
//Works as well    
let fold f a ul =
    let b = []
    let fullList = ul|>List.map( fun {sizeBucket=s; elems = e} -> e@b)|>List.concat
    let applyFuction = fullList|>List.fold(f)a
    applyFuction;;
    
    
//Solution from archieve    
let foldTwo f a ul = ul|>List.fold( fun state {sizeBucket=_; elems = e} -> List.fold f state e )a;;
                              
                              
//Q3

//The function is tail recursive.

(*
I have implemented a accumulator, that accumula
*)

//My soolution
let GA (m, n) =
    let rec inner (m, n) acc =
        match (n,m) with
         |(n,m) when n <= 0 -> (n+m)+acc
         |(n,m) -> inner ((2*m, n-1)) (m+acc) 
    inner (m,n) 0;;
        
let sequence = Seq.initInfinite(fun i -> i )|>Seq.filter(fun i -> i > 0);;     
//My soolution
let mySeq =
    seq { for e1 in sequence  do
    for e2 in sequence do
    yield! [(e1,e2)]};;

let GA2 (m,n) = 
    let rec helper (m,n) acc = if n <= 0 then acc + n + m else helper (2*m,n-1) (acc+m)
    helper(m,n) 0 ;;    

//My soolution
let gSeq = mySeq|>Seq.map(fun (v1,v2) -> GA (v1,v2));;
let gSeqTwo = mySeq|>Seq.map(fun (v1,v2) -> GA2 (v1,v2));;


//Q4
type stack = int list
type inst =
|ADD
| SUB
| PUSH of int
| LABEL of string
| IFNZGOTO of string
| EXIT;;

let insts01 =
[PUSH 10;
PUSH 12;
ADD;
EXIT];;

let insts02 =
[PUSH 10;
LABEL "sub1";
PUSH 1;
SUB;
IFNZGOTO "sub1";
EXIT];;


let execInsts insts =
   let rec exec insts s =
    match (insts,s) with
    | (SUB::is,v1::v2::s) -> exec is (v2-v1::s)
    | (ADD::is,v1::v2::s) -> exec is (v2+v1::s)
    | (PUSH v::is, s) -> exec is (v::s)
    | (EXIT::is,v::s) -> v
    | _ -> failwith "Missing stack values for instruction"
   exec insts [];;
printfn "%A" (execInsts insts01);;    

type resolvedInst =
 | RADD
 | RSUB
 | RPUSH of int
 | RIFNZGOTO of int
 | REXIT;;
type prog = Map<int,resolvedInst>;;

let buildEnv insts =
    let rec build idx env = function
     | [] -> env
     | LABEL lab :: insts -> build idx (Map.add lab idx env) insts
     | _ :: insts -> build (idx+1) env insts
    build 0 Map.empty insts;;

type env = Map<string,int>
let lookup l m =
    match Map.tryFind l m with
    | None -> failwith "Value not in map"
    | Some v -> v;;
let resolveInsts insts env =
    let rec resolve idx = function  
    | [] -> Map.empty
    | PUSH(n) :: insts -> Map.add idx (RPUSH n)  (resolve (idx+1) insts)
    | LABEL lab :: insts -> resolve idx insts
    | SUB :: insts -> Map.add idx RSUB (resolve (idx+1) insts)
    | ADD :: insts -> Map.add idx RADD (resolve (idx+1) insts)
    | IFNZGOTO lab :: insts -> Map.add idx (RIFNZGOTO (lookup lab env)) ((resolve (idx+1) insts))
    | EXIT :: insts -> Map.add idx REXIT (resolve (idx+1) insts);;    
printfn "%A" (resolveInsts insts02 (buildEnv insts02));;    
    
    


    

   