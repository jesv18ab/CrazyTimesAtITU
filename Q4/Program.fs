
let dt(d,m,y) = System.DateTime(y, m, d);;
exception Error of string;;
   
    type Position =
        | Stock of string
        | Cash of float;;
   
    type Action =
        | Aquire of System.DateTime * Position
        | Give of System.DateTime * Position
        | Scale of int * Action
        | All of Action list;;
        
let ex1 = Scale(100,All[Aquire (dt(1,2,2018),Stock "APPLE");Give (dt(1,2,2018),Cash 300.3)]);;

let sellApple =Scale(100,All [Give (dt(1,3,2018),Stock "APPLE"); Aquire (dt(1,3,2018),Cash 400.4)]);;

let tableOfTransactions =  Map[
     ("APPLE",[(dt(1,2,2018),300.3); dt(1,3,2018),400.3])
     ("ISS",[(dt(1,2,2018),150.0); dt(1,3,2018),200.2])
     ("TIVOLI",[(dt(1,2,2018),212.0); dt(1,3,2018),215.2] )
     ];;
let price (s,d) = tableOfTransactions|>Map.find s|>List.filter(fun (k, v) -> k = d)|>List.map(fun (k,v) -> v)|>List.head;;

let buyStock n s d =
    let p = price(s,d)
    let formatted = Scale(n,All[Aquire (d,Stock s);Give (d,Cash p)])
    formatted;;

let receiveCash c d = Aquire (d,Cash 100000.0);;

let actions =
    let d1 = dt(1,2,2018)
    let d2 = dt(1,3,2018)
    All [receiveCash 100000.0 d1;
    buyStock 100 "APPLE" d1;
    buyStock 200 "ISS" d1;
    buyStock 50 "TIVOLI" d2];;
    
type stockEnv = Map<string,int>
    let updStock s n m =
    match Map.tryFind s m with
    | None -> Map.add s n m
    | Some n1 -> Map.add s (n+n1) m;;
type env = float * stockEnv
let emptyEnv = (0.0,Map.empty)

let updEnv scaling (cash,stockEnv) pos =
    match ((pos)) with
    | Cash f -> ((f * (float scaling)), (stockEnv))
    | Stock s -> (0.0, updStock s scaling stockEnv);;
    
let rec execA action env =
    let rec exec scaling env = function
        | Aquire(dateTime,position) -> updEnv scaling env position
        | Give(dateTime, position) -> updEnv scaling env position
        | Scale(i, action) -> exec i env action
        | All actions ->
             let x = actions.Head
             let inner scaling env =
               match x with
                 | Aquire(dateTime,position) -> updEnv scaling env position
                 | Give(dateTime, position) -> updEnv scaling env position
                 | Scale(i, action) -> exec i env action
             inner 1 env    
    exec 1 env action;;

printfn "%A" (execA actions emptyEnv);;