(*
    I hereby declare that I myself have created this exam hand-in in its entirety without help from anybody else.

    Andreas Højgaard Jørgensen @ ITU SD Master degree 
*)

// Question 1.1 

let infSeq3 = Seq.initInfinite (fun i -> i*3 ) // generate infinite seq

let finSeq3 n = Seq.take n infSeq3 // take up to the n value 

let sumSeq3 n = Seq.sum (finSeq3 n) // sum all taken up to the n value

sumSeq3 100

// Question 1.2 

let seqMap2 f s1 s2 =
    seq { for (x,y) in Seq.zip s1 s2 do
            yield f x y }

(*
Make a zip of two seq with generic type and apply a function to each tuple in the zipped seq.
A new seq is returned. 
*)

let swap (x,y) = (y,x)

// seqMap2 swap [1;3;3] [4;5;2]

(*
    Is does not work as f in seqMap2 is expecting two arguments. The given swap contains a single argument.
*)

let fix f x y = f (x,y) // must await to arguments

seqMap2 (fix swap) [1;3;3] [4;5;2]

// Question 2 

type TrieNode<'a when 'a : equality> = TN of 'a * bool * TrieNode<'a> list

let trie01 = TN('a',false,[TN('n',false,[TN('d',true,[])])])

// Question 2.1 

let trie03 = TN('a',false,[TN('n',true,[TN('d',true,[])]);TN('d',false,[TN('d',true,[])]);TN('t',true,[])])
let trie04 = TN('a',false,[TN('n',true,[TN('d',true,[])]);TN('d',false,[TN('d',true,[])]);TN('t',true,[TN('x',false,[])])])

// Type: TriNode<char> 
// Since trie04 has been declared with a char on the generic type 'a, it's now monomorphic.

exception TrieError of string 

// Question 2.2 

let numLetters tn = 
    let rec letters (TN (char, bool, tnl)) count = 
        match (tnl) with 
        | [] -> count
        | head :: tail -> let first = 1 + letters head count // count each branch from root   
                          letters (TN (char, bool, tail)) first // go on to next element in list 
    letters tn 1 // 1 = root node 

let numLetters2 tn = 
    let rec count (TN (char, bool, tnl)) = (List.fold (fun state element -> count element + state + 1) 0 tnl) 
    count tn + 1
    
numLetters2 trie04

let numWords (TN (char, bool, tnl)) = 
    let rec words (TN (char, bool, tnl)) count = 
        match (tnl) with 
        | [] -> count
        | (TN (char, bool, tnl2)) :: tail -> let first = if bool = true then 1 + words (TN (char, bool, tnl2)) count else 0 + words (TN (char, bool, tnl2)) count
                                             words (TN (char, bool, tail)) first
    if (bool = true) then words (TN (char, bool, tnl)) 1 else words (TN (char, bool, tnl)) 0 // To check the root node


// let rec countWords (TN (char, bool, tnl)) = List.fold (fun state (TN (char, bool, tnl)) -> (countWords (TN (char, bool, tnl))) + (if bool = true then state + 1 else 0)) 0 tnl
// countWords trie04

let rec exists lst (TN(char,isWord,children)) =
    match lst with
    | []   -> false     //unclear if "" counts as a word
    | [c]  -> c=char && isWord
    | h::t -> h=char && (List.exists(fun tree -> exists t tree) children)

exists ['a';'n'] trie04
exists ['a';'t';'x'] trie04

let rec chkTrie (TN (char, bool, tnl)) = 
    match tnl with 
    |[] -> if bool = true then true else false  
    |list  -> List.forall (fun element -> chkTrie element = true ) list

chkTrie trie03

// Question 2.3 

let rec map f (TN (char, bool, tnl)) = 
    match tnl with 
    | [] -> (TN (f char, bool, tnl))
    | list -> (TN (f char, bool, List.map (fun element -> map f element ) tnl ))

map ((+)'1') trie01

// Somehow the function have the same input type as output type. So changing the type is not possible in my funciton. 

// Question 3 

// Question 3.1 

let rec F m i n k = if k <= 0 then m else F m i n (k-1) * (1.0+i/n)

// F is not tail-recursince since it's leaving "* (1.0+i/n)" behind in each call. 

// Tail-recursive version: 
let rec FA m i n k  = if k <= 0 then m else FA (m * (1.0+i/n)) i n (k-1)

// Question 3.2 

let tabulate f start step stop = [(start,f start);(step,f step );(stop, f stop)]

tabulate (F 100.0 0.1 1.0) 0 2 4

// Question 3.3 
let nl = System.Environment.NewLine // New line
let prettyPrint xs = List.fold (fun state (k,m) ->  state + string k + " | " + string m + nl) (nl + "x | f(x)" + nl + "--+---" + nl) xs 

prettyPrint [(0, 100.0); (2, 121.0); (4, 146.41)]

// Question 4

let dt(d,m,y) = System.DateTime(y, m, d) 

exception Error of string

type Position =
| Stock of string 
| Cash of float

type Action =
| Aquire of System.DateTime * Position 
| Give of System.DateTime * Position
| Scale of int * Action
| All of Action list

let ex1 = Scale(100,All[Aquire (dt(1,2,2018),Stock "APPLE"); Give (dt(1,2,2018),Cash 300.3)])

let sellApple = Scale(100,All [Aquire (dt(1,3,2018),Cash 400.4); Give (dt(1,3,2018), Stock "Apple")])

