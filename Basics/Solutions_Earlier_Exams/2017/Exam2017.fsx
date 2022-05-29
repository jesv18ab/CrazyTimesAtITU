// Question 1 

type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>

let psEx = PrioritySet ["a";"b";"c"]

let priSetEx =  PrioritySet ["a";"q";"a";"b";"b";"q";"d";"a"]

// Type is: PrioritySet list 

let empty = PrioritySet []

let isEmpty pset = pset = PrioritySet [] 

isEmpty empty

let size (PrioritySet pset) = pset.Length

let contains e (PrioritySet ps) = List.contains e ps

contains "b" psEx

let getPN e (PrioritySet ps) = List.findIndex (fun x -> x = e) ps |> (+) 1

let remove e (PrioritySet ps) = List.filter (fun x -> not(x = e)) ps |> (fun s -> (PrioritySet s) )

remove "b" psEx

let add e (PrioritySet ps) = List.append ps [e] |> List.distinct |> (fun s -> (PrioritySet s) )

add "h" psEx

let map f (PrioritySet ps) = List.map f ps |> (fun s -> (PrioritySet s) )

map (fun (c:string) -> c.ToUpper()) psEx

let cp (PrioritySet ps1) (PrioritySet ps2) = 
    if (ps2.IsEmpty || ps1.IsEmpty) then (PrioritySet []) else 
    [ for i in ps1 do for l in ps2 do yield (i,l) ] |> (fun s -> (PrioritySet s) )

cp (PrioritySet ["a";"b";"c"]) (PrioritySet ["a";"b";"c"])
let emptyS : PrioritySet<string> = PrioritySet []
cp (PrioritySet ["a";"b";"c"]) emptyS

// Question 2 

let f curRow = 
    let rec f' = function 
       | [] -> []
       | [_] -> [1]
       | xs -> let (x1::x2::xs) = xs 
               x1 + x2 :: f' (x2::xs)

    (1 :: f' curRow)

(*
f computes the sum of two number at a time, 
starting with the 1st and 2nd numbers in the list and then 2nd and 3rd element in the list and so on.
The results of the sum is used to create resulting list,
which get appended 1 at the start and the end of the list. 

The reason to this warning is that, is tries to pattern match the content of the list into 1st element, 2nd element and the rest of the list. 
In case of only two element it will only be able to match 1st and 2nd as there is nothing left afterwards. 


*)

let fMatch curRow = 
    let rec fMatch' = function 
       | [] -> []
       | [_] -> [1]
       | x1::x2::xs -> x1 + x2 :: fMatch' (x2::xs)

    (1 :: fMatch' curRow)



let fA' curRow = 
    let rec fMatch' acc row =
       match row with  
       | [] -> acc @ []
       | [_] -> acc @ [1]
       | x1::x2::xs -> fMatch' (acc @ [x1 + x2]) (x2::xs)


    (1 :: fMatch' [] curRow)

// Question 3 


let mySeq s1 s2 =
    seq { for e1 in s1 do
                for e2 in s2 do
                  yield! [e1;e2] }

let s1 = Seq.init 10 id 
let s2 = Seq.init 10 id 
Seq.toList (mySeq s1 s2)


// mySeq return the cartesian product in a seqence of the two input sequences. 

// For "any argument" we now have the following combination of char characters. 
let mySeq1 s1 s2 =
    seq { for e1 in 'A'..'B' do
                for e2 in 'D'..'F' do
                  yield! [e1;e2] }

Seq.toList (mySeq1 s1 s2)

let mySeq2 s1 s2 = 
                    seq { for e1 in s1 do
                                for e2 in s2 do
                                  yield (e1,e2)  }

mySeq2 [1;2] ['A';'B';'C']

// Combining types is only possible on tuples, as their size is predefined. 

let mySeq3 n = Seq.initInfinite (fun i -> (n*n)-n*i)

mySeq3 3

// Question 4

type DataSpec =
  | RangeInt of int * int
  | ChoiceString of string list
  | StringSeq of string
  | Pair of DataSpec * DataSpec
  | Repeat of int * DataSpec
  | Pick of string
  | Label of string * DataSpec

let regBook = [("a1",("cheese",25)); ("a2",("herring",4));("a3",("soft drink",5))]

let reg = Repeat(3,Pair(StringSeq "a",
                  Pair(ChoiceString["cheese";"herring";"soft drink"],
                       RangeInt(1,100))))

let pur = Repeat(2,Pair(RangeInt(1,10),StringSeq "a"))

let rand = System.Random()
let next(i1,i2) = rand.Next(i1,i2) 

let numGen =
    let n = ref 0
    fun () -> n := !n+1; !n

let rec genValue data =
    match data with 
    | RangeInt (i1,i2) -> string (next (i1,i2)) 
    | ChoiceString l -> l.[next(0,l.Length)]
    | StringSeq s -> s + string (numGen ())
    | Pair (d1,d2) -> "(" + genValue d1 + "," + genValue d2 + ")"
    | Repeat (i,d) -> (List.init i (fun non -> genValue d)).ToString ()
    | _ -> "" // Remove incomplete pattern match  

let reg2 = Repeat(3,Pair(Label("articleCode",StringSeq "a"), Pair(ChoiceString["cheese";"herring";"soft drink"],RangeInt(1,100))))

let pur2 = Repeat(2,Pair(RangeInt(1,10), Pick "articleCode"))

type Env = Map<string,string list>

let addToEnv name value (env: Env) = 
                    match (Map.tryFind name env) with 
                    | Some l -> env.Add (name, ( l @ [value] ) ) 
                    | None -> env.Add (name,[value])

let env = addToEnv "x" "42" Map.empty

let env2 = addToEnv "x" "43" env

let pickFromEnv s (env: Env) =
            match (Map.tryFind s env) with 
            | Some l -> List.item (next(0,l.Length)) l
            | None -> failwith "Not found"

pickFromEnv "x" env2

let rec genValue2 (env: Env) (data: DataSpec) = 
    match data with 
    | RangeInt (i1,i2) -> (string (next (i1,i2)),env)
    | ChoiceString l -> (l.[next(0,l.Length)],env)
    | StringSeq s -> (s + string (numGen ()),env)
    | Pair (d1,d2) -> let (s1,e1) = genValue2 env d1
                      let (s2,e2) = genValue2 e1 d2
                      ("(" + s1 + "," + s2 + ")",e2)
    | Repeat (i,d) -> let rec makeList count (data : DataSpec) (env : Env) (list : string list) = 
                            match (list) with 
                            | x when x.Length = count -> (list.ToString (), env) 
                            | x when x.Length < count -> let (s,env1) = genValue2 env data 
                                                         makeList count data env1 (list @ [s])
                            | _ -> ("",env) // Remove incomplete pattern match  
                      makeList i d env []
    | Pick s -> ((pickFromEnv s env),env)
    | Label (s,d) -> let (value,e1) = genValue2 env d
                     let e2 = addToEnv s value e1
                     ((pickFromEnv s e2),e2)


let (v,dEnv) = genValue2 Map.empty reg2

let (v1,dEnv1) = genValue2 dEnv pur2

