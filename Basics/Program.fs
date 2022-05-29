

//recursive
let rec factorial x =
    if x < 1 then 1
    else x * factorial(x-1)

factorial 4
    //1st 4 * factorial(3) = 4 * 6 = 24
    //2nd 3 * factorial(2) = 3 * 2 = 6
    //3rd 2 * factorial(1) = 2 * 1 = 2

let my_list = [1;2;3]

//lambda expression
let my_list2 = List.map (fun x -> x*2) my_list


//pipeline for nesting

//list of even items (the modulo)
[5;6;7;8] 
|> List.filter (fun v  -> (v % 2) = 0 )
//multiples the even numbers by two
|> List.map (fun x -> x*2)
|> printfn "Even doubles : %A"


let mult_numb x = x * 3
let add_numb y = y + 5

let mult_add = mult_numb >> add_numb
let add_mult = mult_numb << add_numb

mult_add 3
add_mult 3

//string execute on each char
let upper = String.collect ( fun c -> sprintf "%c," c) "commas"

let str1 = "TesT"
String.exists(fun c -> System.Char.IsUpper(c)) str1

let string_change = String.init 10 (fun c -> c.ToString())

//looping
let magic_num = "7"
let mutable guess = ""

while not(magic_num.Equals(guess)) do 
    printf "guess"
    guess <- System.Console.ReadLine()

printf "you guessed"

//matching
// let age = 0; 

let grade age = 
    match age with
    | age when age < 5 -> "preschool"
    | 5  -> "kinder"
    | age when ((age > 5) && (age <= 18)) -> (age - 5).ToString()
    | _ -> "grown"

grade 20

//lists
let list1 = [1;2;3;4]

list1 |> List.iter ( printf "Num %i")

let list2 = 5::6::7::[]

let list3 = [1..5]

let list4 = ['a'..'g']

let list5 =  List.init 5 (fun x -> x*2)

let list6 = [ for a in [1..5] do yield (a * a) ]

let list7 = [ for a in [0..20] do if a%2 = 0 then yield a]

let list8 = [ for a in 1..3 do yield! [a..a+2]]

list7.Length
list1.IsEmpty
list4.Item(2)
list2.Head
//everything but 1st item
list1.Tail

let list9 = list3 |> List.filter(fun x -> x % 2 = 0)
let list10 = list1 |> List.map (fun x -> (x*x))
List.sort['x';'a';'s']

List.fold (fun sum elem -> sum + elem ) 0 [1;2;3]

//options

let divide x y =
    match y with
    | 0 -> None
    | _ -> Some(x/y)

if (divide 5 0).IsSome then printf "%A" ((divide 5 0 ).Value)
elif (divide 5 0).IsNone then printf "can't divide"
else printf "someting"

//tuples

let my_data = ("Pat", 26, 12)
let (name, age, _) = my_data


//types
//records - key value pairs

type customer = {
    Name: string;
    Balance: float
}

let pat = { Name = "pat"; Balance = 101.5}
pat.Name
pat.Balance

//sequence
let seq1 = seq {1..100}
let seq2 = seq {0 .. 2 .. 50}
let seqDesc = seq {50..1}

let myFinSeq n m = seq { for i in [n .. m] do yield [n .. i] }
myFinSeq 2 8


Seq.toList seq2 |> List.iter (printf " %i")

let is_prime n =
    let rec check i =
        i > n/2 || (n % i <> 0 && check (i+1)) 
    check 2


is_prime 10


//exceptions
let divide2 x y =
    try 
        if y = 0 then raise(System.DivideByZeroException "Can't" )
        printfn "%i / %i = %i " x y (x / y)
    with 
    | :? System.DivideByZeroException -> printfn "can't divide by 0"

divide2 2 0

//lists are immutable
//tail recursion

let rec even ls =
    match ls with 
    |[] -> []
    |head::tail when head % 2 = 0 -> head::even tail
    |_ :: tail -> even tail

let ls1 = [1;2;3;4;5]

even ls1

//tail recursion with acc
let rec evenAcc ls acc =
    match ls with
    |[]-> List.rev(acc)
    |head::tail when head % 2 = 0 -> evenAcc tail (head::acc)
    |_::tail -> evenAcc tail acc
//to avoid stack overflow
evenAcc ls1 []

//Tail recursion cnt.
//this is without ANY tail rec
let rec addOne (input : int list) : int list =
    match input with
    | [] -> []
    | x :: xs -> (x + 1) :: addOne xs

addOne [1;2;3]

//accumulation

let addOneTail ls =
    let rec addOneTailInner ls acc =
        match ls with
        | [] -> acc
        | x::ls -> addOneTailInner ls (x+1::acc)
    addOneTailInner ls [] |> List.rev

// this reversal is required because the accumulator is built up from the start of the list backwards i.e. the first element from input is stored in acc, then the second element is stored before it, and so on.


addOneTail [1;2]

//cps - continuasion passing style recursion
let sum1 n =
  let rec f n cont =
    match n with
    | 1 -> cont 1
    | n -> f (n-1) (fun n1->cont(n+n1))
  f n id

//1+2+3+4..
let rec fC n y c =
    match y with
    | 0 -> c 1
    | k when k > 0 -> fC n (k-1) (fun r -> c (n * r))
    | _ -> failwith "illegal argument"


//List.collect - applies a function to ever el in list and outputs new list, output can have more elements that the initial one
// so x1 -> y1,y2

List.collect (fun x -> [x;x]) [1;2;3]
//with pipe
[1;2;3] |> List.collect (fun x -> [x;x])

List.collect (fun x -> Seq.toList(x)) ["abc"; "dee"]

//List.fold - list of values that you want to combine to get a value
//uses a list, initial state, function that combines the current state and el from list

//sum of int
List.fold ( fun state int -> state + int) 0 [1;2;3] 

//concatinate string
List.fold (fun state ch -> state + ch) "" ["a";"b";"c"] 

//reverse a list 
List.fold (fun state el -> el::state) [] [1;2;3;4]

//List.map - applies function to each el and returns new list

//squre int
List.map (fun x -> x*x) [1;2;3;4]

//squre root of floats
List.map sqrt [0.25; 0.8; 64]

//length of strings
List.map String.length ["av"; "xxxx"; "hej"]

//List.filter - remove one or more elements, use predicate (function that returns false or true where true adds el to te new list)

List.filter (fun x -> x > 0) [-1;3;-6;7]

//even
List.filter (fun x -> x%2=0) [1;2;3;4;5]

//list of strings starting wth char
List.filter (fun (x : string) -> x.StartsWith('c')) ["come"; "cbs"; "not"; "pati"]

// Seq.Map ->	Builds a new collection whose elements are the results of applying the given function to each of the elements of the collection.


//continuation based tail-recursion from book

let rec bigList n = if n=0 then [] else 1::bigList(n-1)

bigList 10

let rec bigListC n c =
    if n=0 then c []
    else bigListC (n-1) (fun res -> c(1::res))




//  count leafs in binary tree
type BinTree<'a> = 
        | Leaf
        | Node of BinTree<'a> * 'a * BinTree<'a>;;

let rec count = function
    | Leaf -> 0
    | Node(tl,n,tr) -> count tl + count tr + 1


//mathing 

let trysome x =
    match x with 
    | 0 -> 0
    | a -> a*2

trysome 1