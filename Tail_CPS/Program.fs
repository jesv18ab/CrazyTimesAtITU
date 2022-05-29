
let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1);;
   
let fact n =
    let rec factorialT n acc =
        match n with
        | 0 | 1 -> acc
        | _ -> factorialT(n-1) (n*acc)
    factorialT n 1;;
let factTwo (n,m) =
    let rec factA (n,m) acc =
      match (n,m) with
        | (0,m) -> acc
        | (n,m) -> factA (n-1, m) n * acc
    factA (n,m) 1  ;;
    

// factA (5,6)

printfn "%A" ( factTwo (5,1));;
 

     