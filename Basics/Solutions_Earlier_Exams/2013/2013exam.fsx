// Question 1 

type item = {id : int; name : string; price : float} 

type register = item list

let v = [{id=1; name="Milk";        price= 8.75};
         {id=2; name="Juice";       price=16.25};
         {id=3; name="Rye Bread";   price=25.00};
         {id=4; name="White Bread"; price=18.50}]

exception Register of string

let getItembyId (i: int) (r: item list) = 
    match (List.tryFind (fun item -> item.id = i) r) with 
    | Some e -> e
    | None -> raise (Register "Not found")

getItembyId 3 v 

let nextID (r: item list) = if r = [] then 1 else (List.fold (fun acc item -> [item.id] @ acc) [] r) |> List.max |> (+) 1

let nextID2 (r: item list) = (List.fold (fun state element -> if element.id > state then element.id else state) 0 r) + 1

nextID2 v

let addItem n p r = List.append r [{id = (nextID r); name = n; price = p}]

addItem "name" 1.00 v

let deleteItemById i r = List.filter (fun item -> not(item.id = i)) r

let uniqueRegister r = (List.distinctBy (fun element -> element.id ) r).Length = r.Length

uniqueRegister v 

let itemsInPriceRange p d r = List.filter (fun element -> element.price <= (p + d) && element.price >= (p - d)) r

itemsInPriceRange 20.00 5.00 v


// Question 2 

(*
let rec f n m =
    if m=0 then n
    else n * f (n+1) (m-1)

type: int -> int -> int 

f 2 2 computes 24 
f 2 1 computes 6
f 1 1 computes 2
*)


let rec fA acc n m =
    if m=0 then n*acc
    else fA (n*acc) (n+1) (m-1)


fA 1 2 2

(*
let rec z xs ys = 
    match (xs, ys) with
    |([],[]) -> []
    | (x::xs,[]) -> (x,x) :: (z xs ys)
    | ([],y::ys) -> (y,y) :: (z xs ys)
    | (x::xs,y::ys) -> (x,y)::(z xs ys)



Examples of output: 

z [] [] computes []
z [1] [] computes [(1,1)]
z [1] [2] computes [1,1);(2,2)]
z [1;2] [3;4] computes [(1,3);(2,4)]

From the same index of each list, it combines the value returned into a tuple. 
*)




(*
let rec s xs ys = match (xs,ys) with
    | ([],[]) -> []
    | (xs,[]) -> xs
    | ([],ys) -> ys
    | (x::xs,y::ys) -> x::y::s xs ys

s [1;2] [3;4;5;6]

    It alternate between taking the head element from each list, and zip them togehter in a list. 
    When on list bigger than the other, it will append the remaining list the end of the output list.

    z [1;2] [3;4] computes [(1, 3); (2, 4)]
    z [1;2] [3;4;5;6] computes [(1, 3); (2, 4); (5, 5); (6, 6)]
*)

let rec sC xs ys f = match (xs,ys) with
                        | ([],[]) -> f []
                        | (xs,[]) -> f xs
                        | ([],ys) -> f ys
                        | (x::xs,y::ys) -> sC xs ys (fun l -> f(x::y::l))

sC [1;2] [3;4;5;6] id

// Question 3 

// Type: Latex<'a option> 

type Latex<'a> =
  |Section of string * 'a * Latex<'a>
  |Subsection of string * 'a * Latex<'a>
  |Label of string * Latex<'a>
  |Text of string * Latex<'a>
  |Ref of string * Latex<'a>
  |End

let text1 = Section ("Introduction", None, 
             Text ("This is an introduction to ...",
              Subsection ("A subsection", None,
               Text ("As laid out in the introduction we ...",
                End))))

let addSecNumbers text1 = 
    let rec addSecNumbersInner text sec_no sub_no = 
        match text with 
        |End -> End
        |Text (s,l) -> Text (s,addSecNumbersInner l sec_no sub_no)
        |Subsection (s,_,l) -> Subsection (s,((string sec_no) + "." + (string (sub_no + 1))), addSecNumbersInner l sec_no (sub_no + 1))
        |Section (s,_,l) -> let sec = sec_no + 1
                            let sub = 0
                            Section (s,(string sec), addSecNumbersInner l (sec) (sub))
    addSecNumbersInner text1 0 0 

let text2 = Section ("Introduction", None,
             Text ("This is an introduction to ...",
                Subsection ("A subsection", None,
                    Text ("As laid out in the introduction we ...",
                        Subsection ("Yet a subsection", None,
                            Section ("And yet a section", None,
                                Subsection ("A subsection more...", None, End)))))))

addSecNumbers text2 

// Latex<'a> -> Latex<string> 

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

let rec buildLabelEnv l (env: Map<string,string>) =
    match l with 
    | End -> env 
    | Text (_, latex) -> buildLabelEnv latex env
    | Ref (_, latex) -> buildLabelEnv latex env
    | Subsection (_, no, latex) -> latex |> (fun (Label (string1, latex1)) -> buildLabelEnv latex1 (env.Add (string1, no)))
    | Section (_, no, latex) -> latex |> (fun (Label (string1, latex1)) -> buildLabelEnv latex1 (env.Add (string1, no)))
    
buildLabelEnv text3 Map.empty

let nl : string = System.Environment.NewLine

let rec toString l = 
    match l with
    | End -> ""
    | Text (string, latex) -> nl + string + toString latex
    | Ref (string, latex) -> nl + string + toString latex
    | Label (string, latex) -> nl + string + toString latex
    | Subsection (string, no, latex) -> nl + no + " " + string + toString latex
    | Section (string, no, latex) -> nl + no + " " + string + toString latex

toString text3

// type Latex<'a> =
//   |Section of string * 'a * Latex<'a>
//   |Subsection of string * 'a * Latex<'a>
//   |Label of string * Latex<'a>
//   |Text of string * Latex<'a>
//   |Ref of string * Latex<'a>
//   |End

// Question 4 

let mySeq = Seq.initInfinite (fun i -> if i % 2 = 0 then -i else i)

// seq<int> 
List.ofSeq (Seq.take 10 mySeq)

// Question 4.2 

let finSeq n M = Seq.init (M+1) (fun m -> n+2*m)