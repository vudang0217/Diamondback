open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr
open Printf

let boa_max = int_of_float(2.**62.) - 1;;
let boa_min = -int_of_float(2.**62.);;
let valid_id_regex = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let number_regex = Str.regexp "^[+-]?[0-9]+"
let reserved_words = ["let"; "add1"; "sub1"; "isNum"; "isBool"; "if"; "set"; "while"; "def"; "print"]
let reserved_constants = ["true"; "false"; ]
let int_of_string_opt s =
  try Some(int_of_string s) with
  | _ -> None

let rec parse (sexp : Sexp.t) =
  match sexp with 
    | Atom("true") -> EBool(true)
    | Atom("false") -> EBool(false)
    | Atom (s) ->
      (match int_of_string_opt s with
        |None -> if Str.string_match number_regex s 0   (* if it is a number and not presentable*)
                 then failwith (sprintf "Non-representable number %s" s)
                 else (if Str.string_match valid_id_regex s 0 
                      then (if List.mem s reserved_words 
                            then failwith "Invalid parse"
                            else  EId(s)) 
                      else failwith (sprintf "Non-representable number %s" s))
        |Some(i) -> ENumber(i))
    | List([]) -> failwith "Invalid empty"
    | List(sexps) ->
      (match sexps with 
        | [Atom("add1"); arg] ->  EPrim1(Add1, parse arg)
        | [Atom("sub1"); arg] ->  EPrim1(Sub1, parse arg)
        | [Atom("isNum"); arg] ->  EPrim1(IsNum, parse arg)
        | [Atom("isBool"); arg] ->  EPrim1(IsBool, parse arg)
        | [Atom("print");arg] -> EPrim1(Print, parse arg)
        | [Atom("+"); arg1 ; arg2] -> EPrim2(Plus,parse arg1,parse arg2)
        | [Atom("-"); arg1 ; arg2] -> EPrim2(Minus,parse arg1,parse arg2)
        | [Atom("*"); arg1 ; arg2] -> EPrim2(Times,parse arg1,parse arg2)
        | [Atom("<"); arg1 ; arg2] -> EPrim2(Less,parse arg1,parse arg2)
        | [Atom(">"); arg1 ; arg2] -> EPrim2(Greater,parse arg1,parse arg2)
        | [Atom("=="); arg1 ; arg2] -> EPrim2(Equal,parse arg1,parse arg2)
        | [Atom("set"); Atom(id); arg] -> ESet(id, parse arg)
        | [Atom("while"); cond] -> failwith "Invalid while"
        | Atom("while")::cond::rest -> EWhile(parse cond, parse_list rest)
        | [Atom("let") ; List([]) ; _ ] -> failwith "Invalid let empty"
        | [Atom("let") ; List(bindings)] -> failwith "Invalid let no body"
        | Atom("let")::List(bindings)::rest -> ELet(help_parse bindings, parse_list rest)
        | [Atom("if"); arg1; arg2; arg3] -> EIf(parse arg1,  parse  arg2, parse arg3)
        | Atom(func)::args -> EApp(parse_name func, parse_list args)
        | [List(l)] -> parse (List(l))  (*  duplicate parentheses  i.e. ((2)) *)
        | _ -> failwith "Invalid no form inner"
      )
    | _ -> failwith "Invalid no form outer"

and parse_list args = 
  match args with 
    | [] -> []
    | x::xs -> (parse x)::(parse_list xs)

and help_parse bind_list = 
  match bind_list with
      | [] -> []
      | x::xs -> (parse_binding x)::(help_parse xs) 

and parse_binding binding =
  match binding with
    | List([Atom(s); arg]) -> if List.mem s (reserved_words@reserved_constants)
                              then failwith "Invalid parse binding reserved"
                              else (s, parse arg)
    | _ -> failwith "Invalid parse binding"
  
and parse_name s =
  match int_of_string_opt s with
        |None -> if Str.string_match number_regex s 0   (* if it is a number and not presentable*)
                 then failwith (sprintf "Invalid parse name 1")
                 else (if Str.string_match valid_id_regex s 0 (* check for valid id name *)
                      then (if List.mem s reserved_words 
                            then failwith "Invalid parse name 2"
                            else  s) 
                      else failwith (sprintf "Invalid parse name 3 -> %s" s))
        | _ -> failwith "Invalid parse name 4" (* if function name is a number *)

let parse_body body = 
  match body with 
  | [] -> failwith "Invalid parse body" (* empty body *)
  | _ -> parse_list body

let parse_type tp = 
  match tp with 
  | "Num" -> TNum
  | "Bool" -> TBool 
  |  _ -> failwith "Invalid parse type"

let rec parse_args args = 
  match args with 
  | [] -> []
  | Atom(id)::Atom(":")::Atom(tp)::rest ->
      let p_id = parse_name id in
      let p_tp = parse_type tp in 
      (p_id,p_tp)::(parse_args rest)
  | _ -> failwith "Invalid parse args" 

let parse_def sexp =
  match sexp with 
  | List(sexps) -> 
    (match sexps with 
      | Atom("def")::Atom(func)::List(args)::Atom(":")::Atom(tp)::body -> 
          let p_body = parse_body body in
          let p_func = parse_name func in
          let p_args = parse_args args in
          let p_tp = parse_type tp in  
          DFun(p_func,p_args,p_tp,p_body)
      | _ -> failwith "Invalid parse def 1"
    )
  | _ -> failwith "Invalid parse def 2"


let rec parse_program sexps =
  match sexps with
  | [] -> failwith "Invalid: Empty program"
  | [e] -> ([], parse e) (* the last element in the list is the expression, rest is definition *)
  | e::es ->
     let parse_e = (parse_def e) in
     let defs, main = parse_program es in
     parse_e::defs, main
