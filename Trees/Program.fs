type Node<'a, 'b> = {
    Key : 'a
    Value : 'b
    Left : Tree<'a, 'b>
    Right : Tree<'a, 'b>
}
and Tree<'a, 'b> = 
    |Node of Node<'a, 'b>
    |Empty

let emptyNode key value = {
    Key = key
    Value = value
    Left = Tree.Empty
    Right = Tree.Empty
}

let rec insertNode key value  = 
    function 
    |Tree.Empty -> Tree.Node (emptyNode key value)
    |Node node -> 
        if key = node.Key then
            Tree.Node { node with Value = value }
        elif key < node.Key then 
            Tree.Node { node with Left = insertNode key value node.Left }
        else
            Tree.Node { node with Right = insertNode key value node.Right }

let rec searchTree value =
    function 
    |Tree.Empty -> None
    |Node node -> 
        if value = node.Key then Some node.Value
        elif value < node.Key then searchTree value node.Left
        else searchTree value node.Right


let rec between minValue maxValue tree =
    seq {
        match tree with
        |Tree.Empty -> ()
        |Node node -> 
            let keepLeft = node.Key >= minValue 
            let keepRight =  node.Key <= maxValue
            
            if keepLeft && keepRight then yield (node.Key,node.Value)
            if keepLeft then yield! between minValue maxValue node.Left
            if keepRight then yield! between minValue maxValue node.Right
    }
        

let height tree = 
    let rec height tree n = 
        match tree with
        |Tree.Empty -> n
        |Node node -> max (height node.Left n+1) (height node.Right n+1)
    height tree 0

let rec count tree = 
    match tree with
    |Tree.Empty -> 0
    |Node node -> 1 + (count node.Left) + (count node.Right)


let a = 
    insertNode "test" "mitt värde" (Tree.Empty) 
    |> insertNode "test2" "asd"
    |> insertNode "tes" "1231"
    |> insertNode "tes" "replaces"


//Suffix array

between  "te" "test2" a |> Seq.toList


let r = System.Random()
let mutable t = Tree.Empty
for i = 0 to 500000 do
    t <- insertNode (r.Next()) (r.Next()) t
    

height t
count t

between 2348732 234982378 t |> Seq.toList

searchTree 12354123 t




searchTree "tes" a 