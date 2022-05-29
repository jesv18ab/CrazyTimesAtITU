// Question 1 
// type multimap<'a,'b when 'a: comparison> = MMap of Map<'a,list<'b>>

type multimap<'a,'b when 'a: comparison> = MMap of Map<'a,list<'b>>

let ex = MMap (Map.ofList [("record",[50]);("ordering",[36;46;70])])

let studReg = MMap (Map.ofList [("Grete",[]);("Hans",["TOPS"; "HOPS"]);("Peter",["IFFY"]);("Sine",["HOPS"; "IFFY"; "BFNP"])])

let studReg2 = MMap (Map.ofList [("Grete",[]);("Hans",["TOPS"; "HOPS"]);("Peter",["IFFY"]);("Sine",["HOPS"; "IFFY"; "BFNP"])])

studReg = studReg2

// Structural comparision also care about the order of the elements. 
// So the same registrations and switched order return false as below: 

let studReg1 = MMap (Map.ofList [("Grete",[]);("Hans",["TOPS"; "HOPS"]);("Peter",["IFFY"]);("Sine",["HOPS"; "IFFY"; "BFNP"])])

let studReg21 = MMap (Map.ofList [("Grete",[]);("Hans",["HOPS"; "TOPS"]);("Peter",["IFFY"]);("Sine",["HOPS"; "IFFY"; "BFNP"])])

studReg1 = studReg21

let canonical (MMap m) = Map.map (fun k v -> List.sort v ) m

let toOrderedList (MMap m) = Map.toList m

toOrderedList studReg

let newMultimap () = MMap (Map.empty) 

let sizeMultimap (MMap m) = 
                        let keycount = Map.fold (fun state key list -> state + 1 ) 0 m
                        let valuecount = Map.fold (fun state key (value: 'b list) -> state + value.Length ) 0 m
                        (keycount, valuecount)

sizeMultimap studReg

let addMultimap (k:'a) (v: 'b) (MMap m) =
    match (m.TryFind k) with 
    | Some list -> let newList = List.append [v] list |> List.distinct 
                   MMap (Map.add k newList m)
    | None -> MMap (Map.add k [v] m)

sizeMultimap (addMultimap "Sine" "BFNP" studReg) = (4,6)
sizeMultimap (addMultimap "Grete" "TIPS" studReg) = (4,7)
sizeMultimap (addMultimap "Pia" "" studReg) = (5,7)

let removeMultimap (k:'a) (vOpt: 'b option) (MMap m) = 
    match (vOpt) with 
    | Some v -> match (m.TryFind k) with 
                | Some l -> let newlist = List.filter (fun e -> not(v = e)) l 
                            MMap (Map.add k newlist m)
                | None -> (MMap m)
    | None -> MMap (m.Remove k)

sizeMultimap (removeMultimap "Sine" None studReg) = (3,3)
sizeMultimap (removeMultimap "Sine" (Some "PLUR") studReg) = (4,6)
sizeMultimap (removeMultimap "Kenneth" (Some "BLOB") studReg) = (4,6) 
sizeMultimap (removeMultimap "Peter" (Some "IFFY") studReg) = (4,5)

let mapMultimap f (MMap m) = Map.map f m

let foldMultimap f s (MMap m) = Map.fold f s m

// foldMultimap (fun acc k v -> String.length v + acc) 0 studReg

// Question 2 

let rec f i j xs = if xs = [] then
                    [i*j] 
                   else
                    let (x::xs') = xs
                    x*i :: f (i*j) (-1*j) xs'

f 10 1 [1 .. 1]

(*
f computes a list of the multiplication table of 10 with the invariant 
that it alternates between plus and minus due to -1 * j for every iteration.
For the last iteration it prints -10 (the base case) 

1 * 10 = 10 :: f (10*1) (-1*1) xs 
2 * 10 = 20 :: f (10*-1) (-1*-1) xs
3 * -10 = -30 :: (-10*1) (-1*1) xs 

[10; 20; -30; -40; 50; 60; -70; -80; 90; -10]
*)

(*

In case of an empty list there is no explicit pattern match case. 
x::xs' cover elements with at least two elements. 
*)

let rec fMatch i j xs = 
    match xs with 
    | [] -> [i*j] 
    | x::xs' -> x*i :: fMatch (i*j) (-1*j) xs'

let rec fA i j xs acc = 
    match xs with 
    | [] -> acc @ [i*j] 
    | x::xs' -> fA (i*j) (-1*j) xs' (acc @ [x*i])

fMatch 10 1 [1 .. 9] 

// Question 3 

let myFinSeq n m = seq { for i in [n .. m] do yield [n .. i] }

myFinSeq 1 5

(*
Given two n and m, myFinSeq generate a sequence with lists inside.
For every list in the sequence, the lists add the next chronologically number to the list until m is reached. 
*)

myFinSeq 10 14 

// 2 times 


let myFinSeq2 n m = seq {for i in n..m do for j in n..i do j  }

Seq.toList (myFinSeq2 3 6)


let sum xs = List.fold (fun r x -> r+x) 0 xs

let seq4000 = myFinSeq 10 4000

let array4000 = Array.ofSeq seq4000

array4000.Length

// array4000 contains 3991 lists 

let sums = Array.Parallel.map sum array4000

// Question 4 


type JSONlite = Object of list<string * Value>
and Value = | String of string
            | Record of JSONlite
            | Label of string * Value
            | Ref of string

let address = Object [("Street", String "Hansedalen"); ("HouseNo", String "27")]

let person1 = Object [("Name", String "Hans Pedersen");("Address", Label ("Addr1", Record address))]

let person2 = Object [("Name", String "Pia Pedersen"); ("Address", Ref "Addr1")]

let persons = Object [("Person1", Record person1); ("Person2", Record person2)]

let student = Object [("Name", String "Per Simonsen"); ("Field", String "BSWU"); ("Course", Record (Object [("BFNP", String "10"); ("BPRD", String "7")]))] 

// let ppJSONlite json = 

let space n = String.replicate n " "
let nl = System.Environment.NewLine
let quote s = "\"" + s + "\""

// let ppJSONlite (obj:JSONlite) = 

let rec valuefun (string,value) : string =
    match value with 
    | String s -> quote string + " : " + quote s
    | Ref s -> quote string + " : " + quote s
    | Label (s,v) ->  quote string + " : {" + nl + space 4 + valuefun (s,v) + nl + space 2 + "}"
    | Record j -> JSONlite j
and JSONlite (Object json) = nl + "{" + nl + space 2 + 
                             String.concat ("," + nl + space 2 ) (List.map valuefun json) + nl + "}" + nl

    
JSONlite (Object [("Street", Ref "Hansedalen");("as", Label ("Something", Label ("hej", String "hej")))])
JSONlite (Object [("Street", String "Hansedalen");("as", Ref "Hansedalen")])


JSONlite address
JSONlite student
JSONlite person1


let rec build value env =
    match value with 
    | String s -> env
    | Ref s -> env
    | Label (s,v) -> Map.add s v (build v env)
    | Record j -> buildEnv j env
and buildEnv (Object json) env = List.fold (fun env (string,value) -> build value env  ) env json

let env = buildEnv persons Map.empty

let rec build2 value =
    match value with 
    | String s -> value
    | Ref s -> match env.TryFind s with 
                        | Some v -> v
                        | None -> Ref s
    | Label (s,v) -> value
    | Record j -> Record(expandRef j)
and expandRef (Object json) = Object (List.map (fun (string,value) -> (string, build2 value)) json)


expandRef persons