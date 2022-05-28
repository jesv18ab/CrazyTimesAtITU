

open System
open System.Text.Encodings.Web

type TrieNode<'a when 'a : equality> = TN of 'a * bool * TrieNode<'a> list

//These tries were quite difficult, shuld you train these?
let trie03 = TN('a', false, [TN('n', false, [TN('d', true, [])]); TN('d', false, [TN ('d', false, [])]); TN('t', true, [TN('g', true, [])])])

let trie04 = TN('a', false, [TN('n', true, [TN('d', true, [])]); TN( 'd', false, [TN ('d', true, [])]); TN('t', true, [TN('x', false, [])])])

//TrieNode<char>
(*
This type is monomorphic,a s it can only hold char values.
A polymorpchic type does not care for the values is is suppose to hold. 
*)

exception HeapError of string

let rec numLetters trie =
    let rec inner (TN (c, b, tn)) acc =
        match tn with
        |[] -> acc
        | x::xs ->
            let initial = 1+ inner (x) (acc)
            inner (TN (c, b, xs)) (initial)
    inner trie 1;; 


let rec inner (TN (c, b, tn)) acc =
 match tn with
 |[] -> acc
 | x::xs ->
            let bel =  x|> (fun i -> match (i) with TN(c,b,v) -> if (b = true) then true else false)
            let initial = bel|>(fun i -> if i = true then 1+ inner (x) (acc) else inner (x) (acc)  )
            inner (TN (c, b, xs)) (initial);;

let rec numWords trie =
    let numNoRoot = inner trie 0
    match trie with
    |TN(_,b,_) ->
        let added = if b = true then (numNoRoot + 1) else numNoRoot
        (added);;

//The next two functions also count number of word, using in built functions.
let rec countWords (TN (_, _, tnl)) = (List.fold (fun state element ->
        let be = element|>(fun i ->
                              match i with
                              |TN(c, b, t) -> if (b = true) then true else false )
        if (be = true) then (countWords element + state + 1) else countWords element + state) 0 tnl);;         
let numWordsUsingFold tn = 
    let numNoRoot = countWords tn + 0
    match tn with
    |TN(_,b,_) ->
        let added = if b = true then (numNoRoot + 1) else numNoRoot
        (added);;
       
let rec exists lst (TN(char,isWord,children)) =
    match lst with
    | []   -> false     //unclear if "" counts as a word
    | [c]  -> c=char && isWord
    | h::t -> h=char && children|>(List.exists(fun tree -> exists t tree));;
    