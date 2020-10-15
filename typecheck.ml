open Expr
open Printf

let rec find ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
    if y = x then Some(v) else find rest x

(* this is for compile, returns Expr DFun *)
let rec find_def p x =
  match p with
  | [] -> None
  | (DFun(name, args, ret, body) as d)::rest ->
    if name = x then Some(d) else find_def rest x

(* this is for type check, returns (string * (typ list * typ)) *)
let rec tc_find_def p x =
  match p with
  | [] -> None
  | ((name, types) as d)::rest ->
    if name = x then Some(d) else tc_find_def rest x

(* compare type *)
let type_equal type1 type2 = 
  match (type1,type2) with 
  | (TBool, TBool) -> true 
  | (TNum, TNum) -> true
  | (TArr(_), TArr(_)) -> true 
  | (TDummy,_) -> true
  | (_,TDummy) -> true
  | _ -> false 

let rec type_equal_list type_list1 type_list2 = 
  match type_list1, type_list2 with 
    | [],[] -> true
    | (t1::t1s),(t2::t2s) -> if type_equal t1 t2 
                              then type_equal_list t1s t2s 
                              else false

type def_env = (string * (typ list * typ)) list

let build_def_env (defs : def list) : def_env =
  let get_typ (DFun(name, args, ret_typ, body)) = (name, ((List.map snd args), ret_typ)) in
  List.map get_typ defs

let rec tc_e (e : expr) (env : (string * typ) list) (def_env : def_env) : typ =
  let tc_e e env = tc_e e env def_env in
  match e with
  | ENumber(_) -> TNum
  | EBool(_) -> TBool
  | EId(id) -> (match find env id with | Some(s) -> s | None -> failwith "Type mismatch id")
  | EPrim1(op,arg) -> tc_prim1 op arg env def_env
  | EPrim2(op, arg1, arg2) -> tc_prim2 op arg1 arg2 env def_env
  | ESet(id, arg) -> let tc_id = (match find env id with | Some(s)->s | None -> failwith "Type mismatch set") in 
                     let tc_arg = tc_e arg env in 
                     if type_equal tc_id tc_arg
                     then tc_arg 
                     else failwith "Type mismatch set"
  | EIf(cond, arg1, arg2) -> let tc_cond = tc_e cond env in
                             let tc_arg1 = tc_e arg1 env in
                             let tc_arg2 = tc_e arg2 env in
                             (match tc_cond with
                              | TBool -> if type_equal tc_arg1 tc_arg2
                                            then tc_arg1 
                                            else failwith "Type mismatch if"
                              | _ -> failwith "Type mismatch if 2")
  | EWhile(cond, args) -> let tc_cond = tc_e cond env in (* check cond *)
                          let _ = tc_sequence args env def_env in (* check body *)
                          if type_equal tc_cond TBool 
                          then TBool
                          else failwith "Type mismatch while"
  | ELet(bindings, args) -> let new_env = tc_let_env bindings env def_env in
                            tc_sequence args (new_env@env) def_env
  | EApp(func, args) -> (match tc_find_def def_env func with 
                          | None -> failwith (sprintf "Unbound function %s" func)
                          | Some((name,(def_args,ret))) ->
                            let len_args = List.length args in 
                            let len_def_args = List.length def_args in
                            if len_args = len_def_args 
                              then let tp_list_args = List.map (fun x -> tc_e x env) args in
                                   if type_equal_list tp_list_args def_args
                                      then ret
                                      else failwith "Type mismatch : EApp, args type not  match"
                              else failwith "Type mismatch : EApp, length args not match" 
                        )
  | EArr(size, typ) -> let tc_size = tc_e size env in 
                        if type_equal tc_size TNum 
                          then TArr(typ)
                          else failwith "Type mismatch : EArr, size isn't type Num"
  | EIndex(arr, index) -> let tc_arr = tc_e arr env in 
                          let tc_index = tc_e index env in  
                          (match tc_arr with 
                          | TArr(t) -> if type_equal tc_index TNum
                                        then TDummy
                                        else failwith "Type mismatch : EIndex, not a number"
                          | _ -> failwith "Type mismatch : EIndex, not an array") 
  | EUpdate(arr, ind, v) -> let tc_arr = tc_e arr env in 
                              let tc_ind = tc_e ind env in 
                              let tc_val = tc_e v env in 
                              (match (tc_arr, tc_ind) with
                               | (TArr(_),TNum) -> tc_val
                               | _ -> failwith "Type mismatch : EUpdate not an array or number"
                              )   

(* generates new type env *)
and tc_let_env bindings env def_env =
  match bindings with 
  | [] -> []
  | (id,arg)::xs -> let tc_arg = tc_e arg env def_env in
                    let to_env = (id,tc_arg) in 
                    to_env::(tc_let_env xs ((to_env)::env) def_env)

(* fix if time -> make it return the last element only *)
and tc_sequence args env def_env =   
  match args with
    | [a] -> tc_e a env def_env
    | x::xs -> let _ = tc_e x env def_env
               in tc_sequence xs env def_env

(* | x::xs -> (tc_e x env def_env)::(tc_sequence_help xs env def_env) *)

(*
and tc_sequence args env def_env=
  let tc_args = tc_sequence_help args env def_env in
  let reversed_args = List.rev tc_args in (* reverses type list  *)
  let last_type = List.hd reversed_args in (* get the head of reversed list *)
  last_type
*)

and tc_prim1 op arg env def_env=
  match op with 
  | Add1
  | Sub1 -> 
    (match (tc_e arg env def_env) with 
      | TNum -> TNum
      | TDummy -> TNum
      | _ -> failwith "Type mismatch prim1")
  | IsNum 
  | IsBool -> 
    (match (tc_e arg env def_env) with 
      | _ -> TBool)
  | Print -> tc_e arg env def_env
             

and tc_prim2 op arg1 arg2 env def_env= 
  match op with 
  | Plus
  | Minus
  | Times -> let tc_arg1 = tc_e arg1 env def_env in 
             let tc_arg2 = tc_e arg2 env def_env in 
             (match (tc_arg1, tc_arg2) with
             | (TNum,TNum)
             | (TDummy,TNum)
             | (TNum, TDummy) -> TNum
             | (TDummy, TDummy) -> TNum
             | _ -> failwith "Type mismatch prim2")
  | Less
  | Greater -> let tc_arg1 = tc_e arg1 env def_env in 
             let tc_arg2 = tc_e arg2 env def_env in 
             (match (tc_arg1, tc_arg2) with
             | (TNum,TNum) 
             | (TDummy,TNum)
             | (TDummy, TDummy)
             | (TNum, TDummy) -> TBool
             | _ -> failwith "Type mismatch greater")
  | Equal -> let tc_arg1 = tc_e arg1 env def_env in 
             let tc_arg2 = tc_e arg2 env def_env in 
             (match (tc_arg1, tc_arg2) with
             | _ -> TBool)

let rec tc_def_body_list args body def_env ret_typ = 
  match body with 
  | [a] -> if type_equal (tc_e a args def_env) ret_typ then ret_typ else failwith "Type mismatch : tc_def not equal ret"
  | b::bs -> let _ = tc_e b args def_env in tc_def_body_list args bs def_env ret_typ
    
(* type check the the arg in body list one by one and compare to ret_typ, fail if not equal *)
let tc_def def_env (DFun(name, args, ret_typ, body)) =
  tc_def_body_list args body def_env ret_typ
  
  (* match List.map (fun x -> if (tc_e x args def_env) = ret_typ then ret_typ else failwith "Type mismatch : tc_def 2") body with 
  | _ -> TNum *)

let tc_p (defs, main) def_env : typ =
  begin ignore (List.map (tc_def def_env) defs); tc_e main [("input", TNum)] def_env end
