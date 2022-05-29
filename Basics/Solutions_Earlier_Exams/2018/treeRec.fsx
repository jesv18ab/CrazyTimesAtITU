
type Heap<'a when 'a: equality> = 
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>


let ex3 = HP(1,HP(2, HP(3, EmptyHP, EmptyHP),HP(5, EmptyHP, EmptyHP)),HP(4, EmptyHP, EmptyHP))

// Type: Heap<int> 
// The type of ex3 is monomorphic since 'a is declared as integers. 

let empty = EmptyHP

exception HeapError of string 

let isEmpty hp = hp = EmptyHP

let s = HP(2,EmptyHP,EmptyHP)

let rec size hp = 
    match hp with 
    |EmptyHP -> 0 
    |HP (value, hp1, hp2) -> 1 + size hp1 + size hp2

size ex3

let find h = 
    let rec f h = 
        match h with 
            |EmptyHP -> []
            |HP (value, hp1, hp2) -> [value] @ f hp1 @ f hp2 
    List.min (f h)

find ex3

let rec chkHeapProperty h ini = 
     match h with 
     |EmptyHP -> true 
     |HP (value, hp1, hp2) -> (if value > ini then chkHeapProperty hp1 value else false) && (if value > ini then chkHeapProperty hp2 value else false)

chkHeapProperty ex3 0

let rec map f h = 
     match h with 
     |EmptyHP -> h
     |HP (value, hp1, hp2) -> HP (f value, map f hp1, map f hp2)

map ((+) 1) ex3

let s1 = HP(2,HP(3,EmptyHP,EmptyHP),EmptyHP)

let f n = if n % 2 = 0 then n + 10 else n - 10 

chkHeapProperty (map f ex3) 0