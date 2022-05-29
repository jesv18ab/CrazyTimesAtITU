(*
    I hereby declare that I myself have created this exam hand-in in its entirety without help from anybody else.

    Andreas Højgaard Jørgensen @ ITU 
*)


// Question 1 

type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>

let ex = MSet (Map.ofList [("a",1);("b",2);("c",1)])
let ex1 = MSet (Map.ofList [("v",1);("b",2);("c",1)])
// Question 1.1 

let diceSet = MSet (Map.ofList [(1,2);(2,1);(3,5);(5,2);(6,2)])

// Type: Multiset<int> 

// No, since Multiset<'a> is not comparable with functions. 

// Question 1.2

let newMultiset () = (MSet Map.empty)

let isEmpty (MSet ms) = ms.IsEmpty 

isEmpty(newMultiset())

// Question 1.3 

let add key (MSet ms) = 
    match (Map.tryFind key ms) with 
    | Some x -> MSet (Map.add key (x+1) ms)
    | None -> MSet (Map.add key 1 ms)

add "a" ex

// As I understand it, removing one element from a duplicated value, 
// just removes a single multiplicity of an element according to the definiation of a MultiSet. 

let del key (MSet ms) = 
    match (Map.tryFind key ms) with 
    | Some x -> Map.add key (x-1) ms |> Map.filter (fun k v -> v > 0) |> fun s -> (MSet s)
    | None -> (MSet ms) 

del "b" ex

// Question 1.4 

// Helper function 
let rec multiplicity count letter list = 
    match count with 
    | 0 -> list 
    | _ ->  list @ [letter] @ multiplicity (count-1) letter list  

let toList (MSet ms) = Map.fold (fun state key value ->  state @ multiplicity value key []) [] ms

toList ex

let fromList list = List.countBy id list |> Map.ofList |> fun x -> (MSet x)

fromList (toList ex)

Map.toList (ex |> fun (MSet x) -> x)

// Question 1.5

let map f ms = List.map f (toList ms) |> fromList

map (fun (c:string) -> c.ToUpper()) ex

let fold f acc ms = List.fold f acc (toList ms)  

fold (fun acc e -> acc+e) "" ex

// Question 1.6 

let union ms1 ms2 = (toList ms1) @ (toList ms2) |> fromList

let minus ms1 ms2 = List.filter (fun ms2e -> not(List.exists (fun ms1e -> ms1e = ms2e ) (toList ms2)) ) (toList ms1) |> fromList 

minus ex ex1

// Question 2 

let rec f n = if n < 10 then "f" + g (n+1) else "f"
    and g n = if n < 10 then "g" + f (n+1) else "g"


// Question 2.1
(*
    All equal numbers as arguement to f results in a beginning and ending "f".
*)

g 5

// No, since it will always stop at 9. 

// Question 2.2 


let rec fA n acc = if n < 10 then gA (n+1) (acc + "f") else acc + "f"
    and gA n acc = if n < 10 then  fA (n+1) (acc + "g")  else acc + "g" 


gA 5 ""

// Question 3

// Question 3.1


let myFinSeq (n,m) = seq { for i in [0 .. n] do yield! seq { for j in [0 .. m] do yield j }}

List.ofSeq (myFinSeq (3,2))

(*
    n determine the number of repeatet numbers series. 
    m determine the amount of chronological numbers in each series. 

*)

// List.ofSeq (myFinSeq (2,2))
(*
    Above example compute the closest possible seq to the requested seq. 
    It's not possible since, we can't omit the last 2.
*)

// Question 3.2

let myFinSeq2 (n,m) = seq {for i in [0..n] do yield (i, seq {for j in [0..m] do yield j})}

List.ofSeq (myFinSeq2 (1,2))

// Question 4 

type Row = int
type Col = char
type CellAddr = Row * Col
type ArithOp = Add | Sub | Mul | Div 
type RangeOp = Sum | Count

type CellDef =
    | FCst of float
    | SCst of string
    | Ref of CellAddr
    | RangeOp of CellAddr * CellAddr * RangeOp
    | ArithOp of CellDef * ArithOp * CellDef


type CellValue = 
    |S of string
    |F of float

type Sheet = Map<CellAddr,CellDef>


let header = [((1,'A'),SCst "#EYES");((1,'B'),SCst "1");((1,'C'),SCst "2"); 
             ((1,'D'),SCst "3");((1,'E'),SCst "4");((1,'F'),SCst "5"); 
             ((1,'G'),SCst "6");((1,'H'),SCst "Total")]

let result = [((2,'A'),SCst "RESULT");((2,'B'),FCst 2.0);((2,'C'),FCst 1.0); 
            ((2,'D'),FCst 5.0);((2,'E'),FCst 0.0);((2,'F'),FCst 2.0); 
            ((2,'G'),FCst 2.0);((2,'H'),RangeOp((2,'B'),(2,'G'),Sum))]

let calcPct col = ArithOp(FCst 100.0, Mul, ArithOp(Ref(2,col),Div,Ref(2,'H'))) 

let pct = [((3,'A'),SCst "PCT");((3,'B'),calcPct 'B');((3,'C'),calcPct 'C');
          ((3,'D'),calcPct 'D');((3,'E'),calcPct 'E');((3,'F'),calcPct 'F');
          ((3,'G'),calcPct 'G');((3,'H'),calcPct 'H')] 

let dice = Map.ofList (header @ result @ pct)

// Question 4.1 

let name = [((4,'B'),SCst "NAME"); ((5,'B'),SCst "Hans");((6,'B'),SCst "Trine");((7,'B'),SCst "Peter")]
let height = [((4,'C'),SCst "HEIGHT"); ((5,'C'),FCst 167.40 );((6,'C'),FCst 162.30);((7,'C'),FCst 179.70)]
let countName = [(9,'B'),RangeOp((5,'B'),(7,'B'),Count)]
let calcAvg = ArithOp(RangeOp((5,'C'),(7,'C'),Sum),Div,Ref(9,'B'))
let avgHeight = [((9,'C'),calcAvg)]

let heights : Sheet = Map.ofList (name @ height @ countName @ avgHeight)

// Question 4.2 

let getF = function 
    |F f -> f
    |S s -> failwith "getF: expecting a float but got a string"

let evalRangeOp xs op = 
    match op with 
    |Sum -> List.fold (fun state value -> (getF value) + state) 0.00 xs
    |Count -> List.length xs |> float 

evalRangeOp [F 33.0; F 32.0] Sum 
evalRangeOp [] Sum
// evalRangeOp [F 23.0; S "Hans"] Sum
evalRangeOp [F 23.0; S "Hans"] Count

let evalArithOp v1 v2 op =
    match op with 
    |Add -> getF v1 + getF v2 
    |Sub -> getF v1 - getF v2 
    |Mul -> getF v1 * getF v2 
    |Div -> getF v1 / getF v2 

evalArithOp (F 33.0) (F 32.0) Sub
// evalArithOp (S "Hans") (F 1.0) Add

// Question 4.3 

let rec evalValue (value:CellDef) (sheet : Sheet) : CellValue = 
    match value with 
    | FCst float -> F float
    | SCst string -> S string 
    | Ref value -> evalCell value sheet 
    | RangeOp ((r1,c1),(r2,c2), Op) -> if (c1 = c2) // In case of vertial range
                                       then 
                                        let CellAddrlist = List.init ((r2-r1)+1) (fun i -> (r1+i,c1))
                                        let valueList = List.map (fun cellAddr -> evalCell cellAddr sheet) CellAddrlist
                                        (F (evalRangeOp valueList Op))
                                       else // In case of horizontal range 
                                        let CellAddrlist = List.fold (fun state char -> state @ [(r1,char)] ) [] [c1 .. c2]
                                        let valueList = List.map (fun cellAddr -> evalCell cellAddr sheet) CellAddrlist
                                        (F (evalRangeOp valueList Op))
    | ArithOp (v1,op,v2) -> (F (evalArithOp (evalValue v1 sheet) (evalValue v2 sheet) op))
and evalCell (cellAddr: CellAddr) (sheet : Sheet) : CellValue = 
    match (Map.tryFind cellAddr sheet) with 
    | Some value -> evalValue value sheet 
    | None -> S "" // We define an empty cell to be the empty string value.

evalCell (3,'G') dice
evalCell (3,'H') dice
evalCell (9,'C') heights

// Question 4.4 

// let rec ppBoard (sheet : Sheet) count  = if count <= sheet.Count then 
let nl = System.Environment.NewLine + "   "

let cellToString v =
    match v with 
    |S s -> s 
    |F f -> string (System.Math.Round (f,2))

let rec line l =
    match l with 
    |0 -> ""
    |_ -> "------------" + line (l-1) 

let space n = String.replicate (8 - n) " "

let charToNo c = List.findIndex (fun l -> l = c) ['A' .. 'Z'] 

let ppBoard (sheet : Sheet) = let list = Map.toList sheet
                              let ((_,MaxCol), _) = List.maxBy (fun ((row,col), cellDef) -> col ) list
                              List.fold (fun state ((row,col), cellDef) -> 
                              if col = MaxCol then state + cellToString (evalCell (row,col) sheet) + nl + line (charToNo MaxCol) + nl
                              else 
                              let count = (cellToString (evalCell (row,col) sheet)).Length
                              state + "|  " + cellToString (evalCell (row,col) sheet) + (space count)) 
                               "" list

ppBoard dice 
ppBoard heights


seq { for i in 0 .. 10 do yield for j in 1 .. 3 do yield! seq {0 .. 2}}

List.ofSeq (seq {for i in 1 .. 10 do yield! seq {i .. 10}})