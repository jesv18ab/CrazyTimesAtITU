

open Microsoft.FSharp.Collections

type multimap<'a,'b when 'a: comparison> =
    MMap of Map<'a,list<'b>>;;

let ex = MMap (Map.ofList [("record",[50]);("ordering",[36;46;70])]);;


(*
Q1

1.1: No comments needed
*)
let studReg = MMap (Map.ofList [("Grete", [])
                                ("Hans", ["Tops";"Hops"])
                                ("Peter", ["IFFY"])
                                ("Sine", ["BFNP"; "HOPS"; "IFFY"])
                                    ]);;
let studRegUnsorted = MMap (Map.ofList [("Grete", [])
                                        ("Peter", ["IFFY"])
                                        ("Thomas", ["IFFY"])
                                        ("Sine", ["BFNP"; "HOPS"; "IFFY"])
                                    ]);;

(*
Part two:
Not sure what they are looking for
If I change a key, then the same course registrations will ofcause not simplify to true, when compares to studreg
Is that why, A has to be comparable?
*)
let studRegTwo = MMap (Map.ofList [("Grete", [])
                                   ("Hans", ["TOPS;HOPS"])
                                   ("P", ["IFFY"]);
                                   ("Thomas", ["BFNP"; "HOPS"; "IFFY"])
                                   ("Sine", ["BFNP"; "HOPS"; "IFFY"])
                                   ]);;
//printfn "%A" (studReg = studRegTwo);;


//Part three
(*
Sorting functions works just fine. First we sort values for each entry. 
Next we sort by key. In built functions are great
*)
let canonical = function
    | MMap aMap ->
        let values = aMap |> Seq.map(fun (k) -> (k.Key, List.sort k.Value))|> Seq.sortBy id
        (values);;
printfn "%A" (canonical studRegUnsorted);;

