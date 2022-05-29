(* Solutions to exam BFNP-F2015, june 2015 *)





(* Assignment 4 - JSONlite *)

type JSONlite = Object of list<string * Value>

and Value =
    | String of string
    | Record of JSONlite
    | Label of string * Value
    | Ref of string

let address =
    Object [ ("Street", String "Hansedalen")
             ("HouseNo", String "27") ]

let person1 =
    Object [ ("Name", String "Hans Pedersen")
             ("Address", Label("Addr1", Record address)) ]

let person2 =
    Object [ ("Name", String "Pia Pedersen")
             ("Address", Ref "Addr1") ]

let persons =
    Object [ ("Person1", Record person1)
             ("Person2", Record person2) ]

let student =
    Object [ ("Name", String "Per Simonsen")
             ("Field", String "BSWU")
             ("Course",
              Record(
                  Object [ ("BFNP", String "10")
                           ("BPRD", String "7") ]
              )) ]

let nl = System.Environment.NewLine // New line
let space n = String.replicate n " " // Make n spaces
let ppQuote s = "\"" + s + "\"" // Put quotes around string s

let ppJSONlite json =
    let rec ppValue' indent =
        function
        | String s -> ppQuote s
        | Record jslite -> ppJSONlite' indent jslite
        | Label (lab, v) -> lab + " -> " + (ppValue' indent v)
        | Ref lab -> "ref " + lab

    and ppJSONlite' indent =
        function
        | Object xs ->
            let ppkv (k, v) =
                nl
                + (space (indent + 2))
                + (ppQuote k)
                + " : "
                + (ppValue' (indent + 2) v)

            "{"
            + (String.concat "," (List.map ppkv xs))
            + nl
            + (space indent)
            + "}"

    ppJSONlite' 0 json

printfn "\n%s" (ppJSONlite persons)
printfn "\n%s" (ppJSONlite student)

type env = Map<string, Value>

let buildEnv json =
    let rec buildEnvValue env =
        function
        | String s -> env
        | Record jslite -> buildEnvJSONlite env jslite
        | Label (lab, v) -> Map.add lab v (buildEnvValue env v)
        | Ref lab -> env

    and buildEnvJSONlite env =
        function
        | Object xs -> List.fold (fun env (_, v) -> buildEnvValue env v) env xs

    buildEnvJSONlite Map.empty json

buildEnv person1

let expandRef json =
    let env = buildEnv json

    let rec expandRefValue =
        function
        | String s -> String s
        | Record jslite -> Record(expandRefJSONlite jslite)
        | Label (lab, v) -> expandRefValue v // Remove label as we are expanding
        | Ref lab -> Map.find lab env // Remove reference as we are expanding

    and expandRefJSONlite =
        function
        | Object xs -> Object(List.map (fun (k, v) -> (k, expandRefValue v)) xs)

    expandRefJSONlite json

let persons' = expandRef persons
printfn "\n%s" (ppJSONlite persons')
