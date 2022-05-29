
type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>;;
let ex = MSet (Map.ofList [("a",1);("b",2);("c",1)]);;
let diceSet = MSet (Map.ofList [(1,2);(2,1);(3,5);(4,0);(5,2);(6,2)]);;

//The multiset ahs type:Multiset<int>
(*
Meaning it is a multiset containing ints

- tuple of ints
*)


let newMultiset() : Multiset<'a> = Map[]|>MSet;;

let isempty (ms: Multiset<'a>) = 
    match ms with
    |MSet aMap -> if aMap.IsEmpty then true else false
    |_ -> failwith "Error";;
    
    
let add k (ms: Multiset<'a>) = 
    match ms with
    |MSet aMap ->
        let cur = aMap|>Map.find k
        let newVal = cur+1
        aMap|>Map.add k (cur+1)
    | _ -> failwith "Error";;

let del k (ms: Multiset<'a>) =
    match ms with
    |MSet aMap -> if aMap|>Map.containsKey k then aMap|>(Map.remove k)|>MSet else aMap|>MSet

let rec makeList  (k, v) =
    match (k,v) with
    |(k,0) -> []
    |(k,v) -> k::makeList(k,v-1);; 
let toList (ms: Multiset<'a>) = 
    match ms with
    | MSet aMap -> aMap|>Map.toList|>List.map(fun (k,v) -> (makeList (k,v)))|>List.concat
    
let fromList xs = xs|>List.countBy id|>Map|>MSet

let map f ms =
    match ms with
    | MSet aMap -> aMap|>Map.toList|>List.map(fun (k,v) -> ((f k),v))|>Map|>MSet;;
    
let fold f a ms =
     match ms with
     | MSet aMap ->
         let list = toList ms
         (List.fold (f) (a:string) list);;


let union ms1 ms2 =
         let l1 = toList ms1
         let l2 = toList ms2
         let formatted = l2@l1|>List.countBy id
         formatted|>Map|>MSet;;



let minus ms1 ms2 =
     match (ms1, ms2) with
     |(MSet m1, MSet m2)->
        let k1= m1|>Map.toList|>List.map(fun (k,v)-> k)|>List.distinct|>Set.ofList
        let k2= m2|>Map.toList|>List.map(fun (k,v)-> k)|>List.distinct|>Set.ofList
        let k3 = (k2 - k1)
        let formatted= k3|>Set.toList
        let newList = m1|>Map.toList|>List.filter(fun (k,v) -> (List.contains k formatted))|>Map|>MSet
        newList;;
 
 
// All n applied on f that are even and applied will result in a a result string that starts with f an ends with f

// This sequence can never start with a g, as it will start with an f in any case.

//You can't forawrd an infinite integers as it has an upper bound  
(*let rec f n =
    if n < 10 then "f" + g (n+1) else "f"
and g n =
    if n < 10 then "g" + f (n+1) else "g";;*)


let rec fA n acc =
          if (n < 10) then gA (n+1) (acc+"f") else acc + "f"
and     gA n acc =
         if n < 10 then fA (n+1) (acc+"g") else acc + "g"
     
//This sequence computes computes incrementing values from 0 t the friat paramater.
// This is done n amount of times.

//We can never have this sequence as the inner sequence goes from 0  to m. This means it will no terminate, before the
//the last value is computed.    
let myFinSeq (n,m) = seq { for i in [0 .. n] do yield! seq { for j in [0 .. m] do yield j }};;

let makeElement (m) = seq { for j in [0 .. m] do yield j }

//3.2

let rec myFinSeq2 (n,m) =
    let list = [0..n]
    let formatted = List.map(fun i -> (i,(makeElement m))) list
    formatted;;


    type Row = int
    type Col = char
    type CellAddr = Row * Col
    type ArithOp = Add | Sub | Mul | Div
    type RangeOp = Sum | Count
    type CellDef =
        FCst of float
        | SCst of string
        | Ref of CellAddr
        | RangeOp of CellAddr * CellAddr * RangeOp
        | ArithOp of CellDef * ArithOp * CellDef;;
type CellValue =
    S of string
    | F of float
    type Sheet = Map<CellAddr,CellDef>;;

let header = [((1,'A'),SCst "#EYES");((1,'B'),SCst "1");((1,'C'),SCst "2");
((1,'D'),SCst "3");((1,'E'),SCst "4");((1,'F'),SCst "5");
((1,'G'),SCst "6");((1,'H'),SCst "Total")];;
let result = [((2,'A'),SCst "RESULT");((2,'B'),FCst 2.0);((2,'C'),FCst 1.0);
((2,'D'),FCst 5.0);((2,'E'),FCst 0.0);((2,'F'),FCst 2.0);
((2,'G'),FCst 2.0);((2,'H'),RangeOp((2,'B'),(2,'G'),Sum))];;
let calcPct col = ArithOp(FCst 100.0, Mul, ArithOp(Ref(2,col),Div,Ref(2,'H')));;
let pct = [((3,'A'),SCst "PCT");((3,'B'),calcPct 'B');((3,'C'),calcPct 'C');
((3,'D'),calcPct 'D');((3,'E'),calcPct 'E');((3,'F'),calcPct 'F');
((3,'G'),calcPct 'G');((3,'H'),calcPct 'H')];;
let dice = Map.ofList (header @ result @ pct);;

let heights =[((4,'C'),SCst "HEIGHT")
              ((5,'C'),FCst 167.40)
              ((6,'C'),FCst 162.30)
              ((7,'C'),FCst 179.70)
              ((9,'C'),FCst 169.80)
              ];;
let getF = function
F f -> f
| S s -> failwith "getF: expecting a float but got a string";;
let evalRangeOp xs op =
    if List.isEmpty xs then 0.0
    else
        match op with
        |(Sum) -> List.fold( fun state element -> (getF element) + state )0.0 xs
        |(Count) ->List.length xs;;
        
let evalArithOp v1 v2 op =
    match op with
    |Sub -> getF v1 - getF v2
    |Add -> getF v1 + getF v2
 
        

    
    
        
    


