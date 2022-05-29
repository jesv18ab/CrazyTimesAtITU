(*
    I hereby declare that I myself have created this exam hand–in 
    in its entirety without help from anybody else.

    Written by Andreas Højgaard Jørgensen
*)


// Question 1 

type Rope =
    | Leaf of string * int
    | Node of Rope * int * Rope


let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)),20,
                 Node(Leaf("ogr",3),3,Leaf("amming",6)))

// Question 1.1 

let rope2 = Node(Leaf ("_and", 4),4,Node (Leaf ("_very_",6),6,Leaf("much_F#",7)))

// Since there isn't any generic type defined for Rope, it's monomorphic.

let rope3 = Node (Node(Leaf("example_",8),8,Leaf("with_",5)),13,Leaf("5_nodes",7))

// Question 1.2 

let rec length r = 
    match r with 
    |Leaf (string, int) -> int 
    |Node (rope1, int, rope2) -> length rope1 + length rope2

length rope1

let rec flatten r = 
    match r with 
    |Leaf (string, int) -> string 
    |Node (rope1, int, rope2) -> flatten rope1 + flatten rope2

flatten rope1

let rec maxDepth r = 
    match r with 
    |Leaf (string, int) -> 1 // To be the leaf node 
    |Node (rope1, int, rope2) -> let right = 1 + maxDepth rope1 
                                 let left = 1 + maxDepth rope2
                                 List.max [right;left]

let index i r = if i >= (flatten r).Length then failwith "Out of range" else (flatten r).Chars(i)

index 28 rope1 // last index

// Question 1.3 

let concat r1 r2 = Node (r1,length r1,r2)

let nl : string = System.Environment.NewLine // Helper function for prettyPrint
let space n = String.replicate (5*n) " " // Helper function for prettyPrint

let prettyPrint r =
    let rec prettyPrint2 r ind =
        match r with
        |Leaf (text, int) -> (space ind) + "Leaf(" + text + "," + string int + ")"
        |Node (rope1, no, rope2) -> (space ind) + "Node(" + nl + prettyPrint2 rope1 (ind+1) + "," + nl + (space (ind + 1)) + string no + "," + nl + prettyPrint2 rope2 (ind+1) + ")"
    prettyPrint2 r 0

prettyPrint rope1

// Question 2 

type Bucket<'a> = { sizeBucket : int; elems : List<'a>}

type UList<'a> = Bucket<'a> list

let ulist01 = [ { sizeBucket = 4; elems = ['A';'B';'C';'D'] };
                { sizeBucket = 2; elems = ['E';'F'] };
                { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] }; 
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] }; 
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

// Question 2.1 

// The type of ulist01 is Bucket<char> list because the generic type 'a 
// in type Bucket<'a> is declaring the element of list(second record) to be List<'a>. 
// Since ulist01 is delcared with char in that list, the type becomes Bucket<char> list.


let ulist02 = [ { sizeBucket = 3; elems = ['A';'B';'C'] };
                { sizeBucket = 3; elems = ['D';'E';'F'] };
                { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] }; 
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] }; 
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

// I moved the 'D' from first bucket to the second bucket and updated the size accordingly. 

ulist02 = ulist01

// Returns false because of the change I just made. The order of the elements counts in.``.. ..``

let emptyUL () : UList<'a> = []

// Question 2.2 

let sizeUL (ul: Bucket<'a> list) = List.fold (fun state  ({ sizeBucket=size; elems=ele }) -> size + state ) 0 ul

sizeUL ulist01 

let isEmpty (ul: Bucket<'a> list) = ul = []

let existsUL e (ul: Bucket<'a> list) = List.exists (fun ({ sizeBucket=size; elems=eleList }) -> List.exists (fun c -> c = e ) eleList ) ul 

existsUL 'A' (emptyUL())

existsUL 'A' ulist01

let itemUL (ul: Bucket<'a> list) i = List.fold (fun state ({ sizeBucket=size; elems=eleList })-> state @ eleList ) [] ul |> fun l -> l.Item i

itemUL ulist01 5

let filterUL p (ul: Bucket<'a> list) = List.map (fun ({ sizeBucket=size; elems=eleList })-> List.filter p eleList) ul

filterUL(fun e -> e < 'I' ) ulist01

// Question 2.3 

let ulist03Wrong = [ { sizeBucket = 2; elems = [1] }; { sizeBucket = 0; elems = [] }; { sizeBucket = 5; elems = [2;3;4;5;6] } ]

let chkUL (ul: Bucket<'a> list) = List.forall (fun ({ sizeBucket=size; elems=eleList }) -> if size <= 4 && not(eleList = []) && size = eleList.Length  then true else false ) ul

chkUL ulist03Wrong
chkUL ulist01 // return false also due to property 1. 
// From assignment: "For instance, chkUL ulist03Wrong returns false and chkUL ulist01 returns true. - statement is wrong 

let map f (ul: Bucket<'a> list) = List.map (fun ({ sizeBucket=size; elems=eleList }) -> {sizeBucket=size; elems=(List.map f eleList)} ) ul 

map (int) ulist01

let fold f a (ul: Bucket<'a> list) = List.fold (fun state ({ sizeBucket=size; elems=eleList }) -> List.fold f state eleList ) a ul

fold (fun a c -> a+((string)c)) "" ulist01

// Question 3 

let rec G (m,n) = if n <= 0 then n + m else G(2*m,n-1) + m

G(2,2)

// It's not tail-recursive as G leaves (+ m) behind to the heap.

let GA (m,n) = 
    let rec helper (m,n) acc = if n <= 0 then acc + n + m else helper (2*m,n-1) (acc+m)
    helper(m,n) 0 

GA (2,2)

// Question 3.2 

let mySeq = seq { for i in 1 .. 100 do for j in 1 .. 100 do yield (i,j) }

Seq.take 4 mySeq

let gSeq = Seq.map GA mySeq

// Question 4 

type stack = int list

type inst =
    | ADD
    | SUB 
    | PUSH of int
    | LABEL of string
    | IFNZGOTO of string
    | EXIT

let insts01 = [PUSH 10; PUSH 12; ADD; EXIT]

let insts02 = [PUSH 10; LABEL "sub1"; PUSH 1; SUB; IFNZGOTO "sub1";EXIT]

// Question 4.1 

let execInsts insts = 
    let rec exec insts s = 
        match (insts,s) with
        | (SUB::is,v1::v2::s) -> exec is (v2-v1::s)
        | (ADD::is,v1::v2::s) -> exec is (v2+v1::s)
        | (PUSH (value)::is,s) -> exec is ([value] @ s)
        | (EXIT::is,v::s) -> v
        | (LABEL lab::_,s) -> failwith "LABEL not implemented"
        | (IFNZGOTO lab::_,s) -> failwith "IFNZGOTO not implemented" 
        | _ -> failwith "Missing stack values for instruction"
    exec insts []

execInsts insts01

// Question 4.1

type resolvedInst =
    | RADD
    | RSUB
    | RPUSH of int
    | RIFNZGOTO of int
    | REXIT

type prog = Map<int,resolvedInst>

let buildEnv insts =
    let rec build idx env insts = 
        match insts with 
        | [] -> env
        | LABEL lab :: tailIn -> build idx (Map.add lab idx env) tailIn
        | _ :: tailIn -> build (idx+1) env tailIn
    build 0 Map.empty insts

buildEnv insts02

type env = Map<string,int> 

let lookup l m =
    match Map.tryFind l m with
    | None -> failwith "Value not in map"
    | Some v -> v

let resolveInsts insts env =
  let rec resolve idx = function
    [] -> Map.empty
    | LABEL lab :: insts -> resolve idx insts
    | ADD :: insts -> Map.add idx RADD (resolve (idx+1) insts)
    | SUB :: insts -> Map.add idx RSUB (resolve (idx+1) insts)
    | IFNZGOTO lab :: insts -> Map.add idx (RIFNZGOTO (lookup lab env)) (resolve (idx+1) insts)
    | PUSH (n) :: insts -> Map.add idx (RPUSH (n)) (resolve (idx+1) insts)
    | EXIT :: insts -> Map.add idx REXIT (resolve (idx+1) insts)
  in resolve 0 insts


resolveInsts insts02 (buildEnv insts02) 