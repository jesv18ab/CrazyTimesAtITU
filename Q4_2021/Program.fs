

type stack = int list
type inst =
   | ADD
   | SUB
   | PUSH of int
   | LABEL of string
   | IFNZGOTO of string
   | EXIT;;


let execInsts insts =
    let rec exec insts s =
        match (insts,s) with
        | (SUB::is,v1::v2::s) ->exec is (v2-v1::s)
        | (ADD::is,v1::v2::s) -> exec is (v2+v1::s)
        | (PUSH item::is, s) -> exec is (item::s)
        | (LABEL lab::_,s) -> failwith "LABEL not implemented"      
        | (IFNZGOTO lab::_,s) -> failwith "IFNZGOTO not implemented"
        | _ -> s
    exec insts [];;
 
let insts01 =
    [PUSH 10;  
    PUSH 12;
    ADD;
    EXIT];;
    
 let insts02 =
    [PUSH 10;
    LABEL "sub1";
    PUSH 1;
    SUB;
    IFNZGOTO "sub1";
    EXIT];;
type resolvedInst =
    | RADD
    | RSUB
    | RPUSH of int
    | RIFNZGOTO of int
    | REXIT;;
type prog = Map<int,resolvedInst>;;


//IF we we have a label operation, we need to exapnd the mapping, with one operation.
// So we call build, forwards the index as argument and adds the label, with the index into the envirnoment env.
//Finally we forwards the argumetns insts, with one less property
let buildEnv insts =
    let rec build idx env = function
        | [] -> env
        | LABEL lab :: insts -> build (idx) (Map.add lab (idx) env) insts
        | _ :: insts-> build (idx + 1) env insts
    build 0 Map.empty insts;;

type env = Map<string,int>;;
let lookup l m =
    match Map.tryFind l m with
        | None -> failwith "Value not in map"
        | Some v -> v;;
let resolveInsts insts env =
     let rec resolve idx = function
        | [] -> Map.empty
        | LABEL lab :: insts -> resolve idx insts
        | ADD :: insts -> Map.add idx RADD (resolve (idx+1) insts)
        | SUB :: insts -> Map.add idx RSUB (resolve (idx+1) insts)
        | PUSH i :: insts -> Map.add idx (RPUSH i )  (resolve (idx+1) insts)
        | IFNZGOTO lab :: insts -> Map.add idx (RIFNZGOTO (lookup lab env)) (resolve (idx+1) insts)
        | EXIT :: insts -> Map.add idx REXIT (resolve (idx+1) insts)
     resolve 0 insts;;
     
     
printfn "%A" (resolveInsts insts02 (buildEnv insts02));;     
     