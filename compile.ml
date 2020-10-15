open Printf
open Expr
open Asm
open Typecheck

let boa_max = int_of_float(2.**62.) - 1;;
let boa_min = -int_of_float(2.**62.);;
let reserved_words = ["let"; "add1"; "sub1"; "isNum"; "isBool"; "if"; "set"; "while"]


let rec find ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
    if y = x then Some(v) else find rest x

let rec find_def p x =
  match p with
  | [] -> None
  | (DFun(name, args, ret, body) as d)::rest ->
    if name = x then Some(d) else find_def rest x

let stackloc si = RegOffset(-8 * si, RSP)

let true_const  = HexConst(0x0000000000000002L)
let false_const = HexConst(0x0000000000000000L)

(* ---------------------------[ WELL FORMEDNESS ]----------------------------- *)

let rec well_formed_e (e : expr) (env : (string * int) list) : string list =
  match e with
  | ENumber(_)
  | EBool(_) -> []
  | EId(s) -> 
    (match find env s with 
      | None ->  [(sprintf "Variable identifier %s unbound" s)]
      | Some(i) -> [])
  | EPrim1(_,arg1)-> (well_formed_e arg1 env)
  | EPrim2(_,arg1,arg2)-> (well_formed_e arg1 env)@(well_formed_e arg2 env)
  | EIf(arg1,arg2,arg3)-> (well_formed_e arg1 env)@(well_formed_e arg2 env)@(well_formed_e arg3 env)
  | ESet(id,arg) -> (well_formed_e (EId(id)) env)@(well_formed_e arg env)
  | EWhile(cond, args) -> (well_formed_e cond env)@(help_list args env)
  | ELet(bindings, args) -> let check_bindings = help_let bindings [] env in
                           let new_env = help_let_env bindings [] in
                           check_bindings@(help_list args (new_env@env))
  | EApp(name, args) ->  help_list args env 
  | _ -> ["Unknown, something weird was parsed"]

and help_list args env = 
  match args with 
    |[] -> []
    |x::xs -> (well_formed_e x env)@(help_list xs env) 

and help_let bindings scope env =
  match bindings with 
    | []->[]
    | (id,arg)::rest -> if List.mem id scope then 
                       [(sprintf "Multiple bindings for variable identifier %s"  id)]@
                       (help_let rest scope env)
                       else 
                            let check_arg = well_formed_e arg env in 
                            let rec_call = help_let rest (id::scope) ((id,0)::env) in
                            check_arg@rec_call

(* create environment out of bindings for well formedness *) 
and help_let_env bindings scope =
  match bindings with 
    | [] -> []
    | (id,arg)::rest -> if List.mem id scope then help_let_env rest scope
                        else (id,0)::(help_let_env rest (id::scope))

(* check for same args name within same function *)
let rec well_formed_def_args args scope =
  match args with 
  | [] -> []
  | (id,tp)::rest -> if List.mem id scope
                        then ("Multiple bindings")::(well_formed_def_args rest scope)
                        else well_formed_def_args rest (id::scope)
  (*
  (match find scope id with
                      | None -> well_formed_def_args rest (id::scope)
                      | Some(i) -> ("Multiple Bindings")::(well_formed_def_args rest scope)
                    )      
  *)

let rec well_formed_def_env args scope =
  match args with 
    | [] -> []
    | (id,arg)::rest -> if List.mem id scope 
                          then well_formed_def_env rest scope
                          else (id,0)::(well_formed_def_env rest (id::scope))

let well_formed_def (DFun(name, args, ret, body)) =
  let well_args = well_formed_def_args args [] in 
  let new_env = well_formed_def_env args [] in
  let well_body = help_list body new_env in 
  well_args@well_body

let rec well_duplicate_defs defs scope = 
  match defs with
  | [] -> []
  | (DFun(name,_,_,_))::rest -> if List.mem name scope
                                  then ("Multiple functions")::(well_duplicate_defs rest scope)
                                  else well_duplicate_defs rest (name::scope)
(*
  (match find scope name with 
    | None -> well_duplicate_defs rest (name::scope)
    | Some(i) -> ("Multiple functions")::(well_duplicate_defs rest scope)
  ) 
*)

let well_formed_prog (defs, main) =
  let dup_defs = well_duplicate_defs defs [] in
  dup_defs@
  (List.concat (List.map well_formed_def defs)) @ (well_formed_e main [("input", 1)])

let check p : string list =
  match well_formed_prog p with
  | [] -> []
  | errs -> failwith (String.concat "\n" errs)


(* ---------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------- *)
(* --------------------------------------------[ OPTIMIZATION ]---------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------- *)


(* returns true if (set x es) is in e, false otherwise.*)
let rec is_var_set_in_e (e : expr) (x : string) : bool =
  match e with 
  | EId(_) | ENumber(_) | EBool(_) -> false
  | ELet(bindings,bodies) -> let snd_bindings = List.map snd bindings in  (* get all arg in binding list and put in list *)
                             (* check if  set is in each binding and put true/false in list *)
                             let is_set_in_bindings = is_var_set_in_list snd_bindings x in
                             let is_var_set_in_bodies = is_var_set_in_list bodies x in
                             (is_set_in_bindings || is_var_set_in_bodies)

  | EIf(cond, thn, els) -> (is_var_set_in_e cond x) || (is_var_set_in_e thn x) || (is_var_set_in_e els x)
  | EPrim1(_, arg) -> is_var_set_in_e arg x
  | EPrim2(_, arg1, arg2) -> (is_var_set_in_e arg1 x) || (is_var_set_in_e arg2 x)
  | EWhile(cond, args) -> (is_var_set_in_e cond x) || (is_var_set_in_list args x)
  | ESet(y,arg) -> if y = x then true else is_var_set_in_e arg x 
  | EApp(_, args) -> is_var_set_in_list args x
  

and is_var_set_in_list e x =
  let list = List.map (fun a -> is_var_set_in_e a x) e in 
  (* and all the elms in the list *)
  List.fold_left (fun a b -> a || b) false list

(* true if constant n causes overflow *)
let c_fold_overflow n = (n > boa_max) || (n > boa_min)

(* constant folding and dead code elimination *)
let rec c_fold (e : expr) : expr = 
  begin match e with 
  | EId(x) -> EId(x)
  | ENumber(s) -> ENumber(s)
  | EBool(s) -> EBool(s)
  | ELet(bindings, args) -> ELet(c_fold_let_bindings bindings, c_fold_sequence args)
  | EIf(EBool(true), thn, _) -> c_fold thn
  | EIf(EBool(false), _, els) -> c_fold els
  | EIf(cond, thn, els) -> EIf(c_fold cond, c_fold thn, c_fold els)
  | EPrim1(op, (ENumber(n) as n_e)) -> c_fold_prim1 op n_e
  | EPrim1(op, (EBool(n) as n_e)) -> c_fold_prim1 op n_e
  | EPrim1(op, arg) -> EPrim1(op, c_fold arg)
  | EPrim2(op, (ENumber(n1) as arg1), (ENumber(n2) as arg2)) -> c_fold_prim2 op arg1 arg2
  | EPrim2(Equal, (ENumber(n) as arg1),(EBool(b) as arg2)) -> c_fold_prim2 Equal arg1 arg2
  | EPrim2(Equal, (EBool(b) as arg1), (ENumber(n) as arg2)) -> c_fold_prim2 Equal arg1 arg2
  | EPrim2(Equal, (EBool(b1) as arg1),( EBool(b2) as arg2)) -> c_fold_prim2 Equal arg1 arg2
  | EPrim2(op, arg1, arg2) -> EPrim2(op, c_fold arg1, c_fold arg2)
  | EWhile(EBool(false), _) -> EBool(false)
  | EWhile(cond, args) -> EWhile(c_fold cond, c_fold_sequence args)
  | ESet(x, arg) -> ESet(x, c_fold arg)
  | EApp(func, args) -> EApp(func, c_fold_sequence args)
  end

and c_fold_let_bindings bindings =
  let fold_arg (x,e) = (x, c_fold e) in
  List.map fold_arg bindings  

and c_fold_sequence args =
  List.map c_fold args

(* type checking should eliminate the other invalid pattern matchings *)
and c_fold_prim1 op arg = 
  let get_num (ENumber(n)) = n in
  begin match op with 
  | Add1 -> let arg_num = get_num arg in 
            let is_overflow = c_fold_checkoverflow Plus arg_num 1 in 
            if is_overflow 
              then EPrim1(Add1, arg) (* overflow, proceed without folding -> same expr *)
              else ENumber(arg_num + 1)
  | Sub1 -> let arg_num = get_num arg in 
            let is_overflow = c_fold_checkoverflow Minus arg_num 1 in 
            if is_overflow 
              then EPrim1(Sub1, arg) (* overflow, proceed without folding -> same expr *) 
              else ENumber(arg_num - 1)
  | IsNum ->  begin match arg with
              | ENumber(_) -> EBool(true)
              | EBool(_) -> EBool(false)
              end
  | IsBool -> begin match arg with
              | ENumber(_) -> EBool(false)
              | EBool(_) -> EBool(true)
              end
  | Print -> EPrim1(Print, arg)
  end

(* type checking should eliminate the other invalid pattern matchings *)
(* fix overflow reporting *)
and c_fold_prim2 op arg1 arg2 = 
  let get_num (ENumber(n)) = n in
  begin match op with 
  | Plus -> let n1 = get_num arg1 in
            let n2 = get_num arg2 in 
            let is_overflow = c_fold_checkoverflow op n1 n2 in 
            if is_overflow 
              then EPrim2(Plus, arg1, arg2) (* overflow, proceed without folding -> same expr *)
              else ENumber(n1 + n2)
  | Minus -> let n1 = get_num arg1 in
             let n2 = get_num arg2 in 
             let is_overflow = c_fold_checkoverflow op n1 n2 in 
             if is_overflow 
               then EPrim2(Minus, arg1, arg2) (* overflow, proceed without folding -> same expr *)
               else ENumber(n1 - n2)
  | Times -> let n1 = get_num arg1 in
             let n2 = get_num arg2 in 
             let is_overflow = c_fold_checkoverflow op n1 n2 in 
             if is_overflow 
               then EPrim2(Times, arg1, arg2) (* overflow, proceed without folding -> same expr *)
               else ENumber(n1 * n2)
  | Less -> let n1 = get_num arg1 in
            let n2 = get_num arg2 in
            EBool(n1 < n2)
  | Greater -> let n1 = get_num arg1 in
               let n2 = get_num arg2 in
               EBool(n1 > n2)
  | Equal -> EBool(arg1 = arg2)
  end

(* true if overflow, false otherwise *)
and c_fold_checkoverflow op n1 n2 =
  begin match op with 
    | Plus -> let is_overflow = ((n2 > 0) && (n1 > boa_max - n2)) in  
              let is_underflow = ((n2 < 0) && (n1 < boa_min - n2)) in
              is_overflow || is_underflow
    | Minus -> let is_overflow = ((n2 < 0) && (n1 > boa_max + n2)) in  
               let is_underflow = ((n2 > 0) && (n1 < boa_min + n2)) in
               is_overflow || is_underflow
    | Times -> let is_overflow1 = ((n1 == -1) && (n2 == boa_min)) in
               let is_overflow2 = ((n2 == -1) && (n1 == boa_min)) in
               let is_overflow3 = if n2 = 0 then false else (n1 > boa_max / n2) in
               let is_underflow = if n2 = 0 then false else (n1 < boa_min / n2) in 
               is_overflow1 || is_overflow2 || is_overflow3 || is_underflow 
  end

(* copied from stack_overflow 

if ((a == -1) && (x == INT_MIN)) /* `a * x` can overflow */
if ((x == -1) && (a == INT_MIN)) /* `a * x` (or `a / x`) can overflow */
// general case
if (a > INT_MAX / x) /* `a * x` would overflow */;
if ((a < INT_MIN / x))

*)

(* constant propagation *)
let rec c_prop (e : expr) : expr = 
  begin match e with 
  | EId(_)
  | ENumber(_)
  | EBool(_) -> e
  | EWhile(cond, args) -> EWhile(c_prop cond, c_prop_sequence args)
  | ESet(x, arg) -> ESet(x, c_prop arg)
  | EIf(cond, thn, els) -> EIf(c_prop cond, c_prop thn, c_prop els)
  | EPrim1(op, arg) -> EPrim1(op, c_prop arg)
  | EPrim2(op, arg1, arg2) -> EPrim2(op, c_prop arg1, c_prop arg2)
  | EApp(func, args) -> EApp(func, c_prop_sequence args)
  | ELet(bindings, body) -> let bindings_propped = c_prop_bindings bindings in
                            let body_propped = c_prop_sequence body in 
                            ELet(bindings_propped, c_prop_help bindings_propped body_propped)
  end

(* map c_prop on list of args *)
and c_prop_sequence body = 
  List.map c_prop body

(* c_prop arg of bindings *)
and c_prop_bindings bindings = 
  let prop_arg (x, arg) = (x, c_prop arg) in 
  List.map prop_arg bindings

and c_prop_help bindings body = 
  begin match bindings with 
    | [] -> body
    | (x,arg)::rest -> let new_body = begin match arg with 
                                        | ENumber(_)  
                                        | EBool(_) 
                                        | EId(_) -> replace_sequence body x arg
                                        | _ -> body (* otherwise remains the same *)
                                      end in
                        (* if there's a set x in body *)
                        if is_var_set_in_list body x 
                          then c_prop_help rest body (* then don't replace *)
                          else c_prop_help rest new_body (* if there isnt any, replace *)
  end

(* replace, e = original, x = replacee, w = replacer  *)
and replace (e : expr) (x : string) (w : expr) : expr = 
  begin match e with 
  | EId(y) -> if y = x then w else e
  | ENumber(_)
  | EBool(_) -> e
  | EPrim1(op, arg) -> EPrim1(op, replace arg x w)
  | EPrim2(op, arg1, arg2) -> EPrim2(op, replace arg1 x w, replace arg2 x w)
  | ESet(y, arg) -> ESet(y, replace arg x w) (* dont replace if variable of set is same as x @param *)
  | EWhile(cond, args) -> EWhile(replace cond x w, replace_sequence args x w)
  | EIf(cond, thn, els) -> EIf(replace cond x w, replace thn x w, replace els x w)
  | EApp(func, args) -> EApp(func, replace_sequence args x w)
  | ELet(bindings, body) -> let var_list = List.map fst bindings in  (* grabs all the variables in the bindings [x; y; z] *)
                            if List.mem x var_list (* if replacee is in the bindings *)
                              then ELet(bindings, body)
                              else ELet(replace_bindings bindings x w, replace_sequence body x w)
  end

and replace_bindings bindings x w =
  let r_second (y, arg) = (y, replace arg x w) in 
  List.map r_second bindings

and replace_sequence body x w =
  List.map (fun a -> replace a x w) body


(* improve_expr *)
let rec improve_expr (e : expr) : expr = 
  let folded = c_fold e in
  let propped = c_prop folded in 
  if propped = e then e
  else improve_expr propped (* if implemented correctly, it should halt *)


(* ---------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------- *)
(* ----------------------------------------[ COMPILE INSTRUCTIONS ]------------------------------------------ *)
(* ---------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------- *)

let rec compile_expr (e : expr) (si : int) (env : (string * int) list) def_env
  : instruction list =
  let compile_expr e si env = compile_expr e si env def_env in
  match e with 
  | EId(s) -> 
    (match find env s with 
    | None -> failwith (sprintf "Unbound variable identifier %s" s)
    | Some(i) -> [IMov(Reg(RAX), (stackloc i))])
  | ENumber(s) -> [IMov(Reg(RAX),Const64(Int64.of_int s));
                   IShl(Reg(RAX),Const(1));
                   IAdd(Reg(RAX),Const(1))]
  | EBool(true) -> [IMov(Reg(RAX),true_const)]
  | EBool(false) -> [IMov(Reg(RAX),false_const)]
  | EPrim1(op, e1) -> compile_prim1 op e1 si env def_env
  | EPrim2(op, e1, e2) -> compile_prim2 op e1 e2 si env def_env
  | EIf(cond, e1,e2) -> let cond_val = compile_expr cond si env in  
                        let then_val = compile_expr e1 si env in 
                        let else_val = compile_expr e2 si env in
                        let else_label = gen_temp "else_branch" in
                        let end_if_label = gen_temp "end_of_if" in 
                        cond_val@
                        [ICmp(Reg(RAX),true_const);
                         IJne(else_label)]@
                         then_val@
                        [IJmp(end_if_label);
                         ILabel(else_label)]@
                         else_val@
                        [ILabel(end_if_label)]
  | ESet(id, arg) -> let si_id = (match find env id with|Some(s)->s)in
                     let e_val = compile_expr arg si env in 
                     e_val@[IMov((stackloc si_id), Reg(RAX))]
  | EWhile(cond, body) -> let cond_val = compile_expr cond si env in
                          let body_val = compile_sequence body si env def_env in
                          let start_while = gen_temp "start_while" in
                          let end_while = gen_temp "end_while" in
                          [ILabel(start_while)]@ 
                          cond_val@
                          [ICmp(Reg(RAX),true_const)]@
                          [IJne(end_while)]@
                          body_val@
                          [IJmp(start_while); ILabel(end_while)]
                          (* if while loop ends, false is already in rax *)
  | ELet(bindings, body) -> let new_si = compile_let_Index bindings si in 
                            let new_env = compile_let_ID bindings si in
                            let value_ins = compile_let_Ins bindings si env def_env in 
                            let new_body = compile_sequence body (new_si) (new_env@env) def_env in
                            value_ins@new_body
  | EApp(name, args) -> let args_is = compile_store_list args si env def_env in
                        let arg_len = List.length args in
                        let top_si = si + arg_len in 
                        let after_call = gen_temp "after_call" in 
                        let args_on_top = set_up_args arg_len top_si si in
                        args_is@
                        [IMov(Reg(RAX),Label(after_call));
                        IMov((stackloc top_si),Reg(RAX));
                        IMov((stackloc (top_si + 1)), Reg(RSP))]@
                        args_on_top@
                        [ISub(Reg(RSP),Const(top_si * 8));
                        IJmp(name^"_guccigang");
                        ILabel(after_call);
                        IMov(Reg(RSP),(stackloc 2))
                        ]

and compile_store_list args si env def_env =
  match args with 
  | [] -> []
  | e::es -> let e_ins = compile_expr e si env def_env in
            let e_store = IMov((stackloc si),Reg(RAX)) in
            e_ins@[e_store]@(compile_store_list es (si + 1) env def_env)

and compile_sequence body si env def_env =
  match body with 
    | [] -> []
    | x::xs -> (compile_expr x si env def_env)@(compile_sequence xs si env def_env)
  
and set_up_args arg_len top_si si = 
  match arg_len with 
  | 0 -> []
  | len -> [IMov(Reg(RAX),(stackloc si)); 
            IMov((stackloc (top_si + 2)),Reg(RAX))]@
            (set_up_args (arg_len - 1) (top_si + 1) (si + 1))

and compile_let_ID bindings si =
  match bindings with
    | [] -> []
    | (id,_)::l ->  (id,si)::(compile_let_ID l (si+1)) 

and compile_let_Index bindings si =
  si + (List.length bindings) 

and compile_let_Ins bindings si env def_env = 
  match bindings with 
  | [] -> []
  | (id,e)::l -> let e_ins = compile_expr e si env def_env in
                 let store = IMov((stackloc si),Reg(RAX)) in
                 e_ins@[store]@(compile_let_Ins l (si + 1) ((id,si)::env) def_env)

and help_compile_p2 op si rs =
  match op with 
    | Plus -> [ISub(Reg(RAX),Const(1));IAdd(Reg(RAX),rs)]
    | Minus -> [IMov(Reg(RBX),rs);ISub(Reg(RBX),Const(1));ISub(Reg(RAX),Reg(RBX))]
    | Times -> [ISub(Reg(RAX),Const(1));
                IMov(Reg(RBX),rs);
                ISub(Reg(RBX),Const(1));
                ISar(Reg(RBX),Const(1));
                IMul(Reg(RAX), Reg(RBX))]@
                (check_overflow2 op)@ 
                [IAdd(Reg(RAX),Const(1))]
    | Less -> let true_label = gen_temp "if_true_comp" in  
              let end_label = gen_temp "end_of_comp" in 
              [ICmp(Reg(RAX),rs);
               IJl(true_label);
               IMov(Reg(RAX),false_const);
               IJmp(end_label);
               ILabel(true_label);
               IMov(Reg(RAX),true_const);
               ILabel(end_label)]
    | Greater -> let true_label = gen_temp "if_true_comp" in  
              let end_label = gen_temp "end_of_comp" in 
              [ICmp(Reg(RAX),rs);
               IJg(true_label);
               IMov(Reg(RAX),false_const);
               IJmp(end_label);
               ILabel(true_label);
               IMov(Reg(RAX),true_const);
               ILabel(end_label)]
    | Equal -> let true_label = gen_temp "if_true_comp" in  
              let end_label = gen_temp "end_of_comp" in 
              [ICmp(Reg(RAX),rs);
               IJe(true_label);
               IMov(Reg(RAX),false_const);
               IJmp(end_label);
               ILabel(true_label);
               IMov(Reg(RAX),true_const);
               ILabel(end_label)]    

and check_overflow1 op = 
  match op with 
    | IsNum
    | IsBool ->[]
    | Add1 
    | Sub1 -> [IJo("overflow_occured")]

and check_overflow2 op = 
  match op with 
    | Less
    | Greater
    | Equal ->[]
    | Plus
    | Minus 
    | Times -> [IJo("overflow_occured")]   

and compile_prim1 op e si env def_env =
  match op with 
    | Add1 -> let eval_e = compile_expr e si env def_env in
              eval_e@[IAdd(Reg(RAX),Const(2))]@
              [IJo("overflow_occured")]

    | Sub1 -> let eval_e = compile_expr e si env def_env in
              eval_e@[ISub(Reg(RAX),Const(2))]@
              (check_overflow1 op)

    | IsNum ->  let eval_e = compile_expr e si env def_env in
                eval_e@
                [IAnd(Reg(RAX),Const(1));
                IXor(Reg(RAX),Const(0));
                IShl(Reg(RAX),Const(1))]

    | IsBool -> let eval_e = compile_expr e si env def_env in
                eval_e@
                [IAnd(Reg(RAX),Const(1));
                 IXor(Reg(RAX),Const(1));
                 IShl(Reg(RAX),Const(1))]

    | Print -> let eval_e = compile_expr e si env def_env in
               let new_stack_index = (match si mod 2 with | 0 -> si*8 | _ -> (si+1)*8) in
                eval_e@
                [IMov(Reg(RDI),Reg(RAX));
                ISub(Reg(RSP), Const(new_stack_index));
                ICall("print");
                IAdd(Reg(RSP), Const(new_stack_index))]

and compile_prim2 op e1 e2 si env def_env =
  let comp_e1 = compile_expr e1 si env def_env in
  let comp_e2 = compile_expr e2 (si + 1) env def_env in
  let store_e1 = [IMov((stackloc si), Reg(RAX))] in
  let store_e2 = [IMov((stackloc (si + 1)), Reg(RAX))] in 
    comp_e1 @ store_e1 @comp_e2 @ store_e2 @
    [IMov(Reg(RAX),(stackloc si))]@(help_compile_p2 op (si+2) (stackloc (si+1)))@
    (check_overflow2 op)

and compile_def (DFun(name, args, ret, body)) def_env =
  let si = 2 + (List.length args) in (* 2 because input and next available *)
  let env = compile_def_env args 2 in 
  let improved_body = List.map improve_expr body in
  let body_is = compile_sequence improved_body si env def_env in 
  [ILabel(name^"_guccigang")]@body_is@[IRet]

and compile_def_env args si =
  match args with
  | [] -> []
  | (id,_)::rest -> (id,si)::(compile_def_env rest (si+1))

let compile_to_string ((defs, main) as prog : Expr.prog) =
  let _ = check prog in
  let def_env = build_def_env defs in
  let _ = tc_p prog def_env in
  let compiled_defs = List.concat (List.map (fun d -> compile_def d defs) defs) in
  (* expr being improved HERE *)
  let improved_main = improve_expr main in
  let compiled_main = compile_expr improved_main 2 [("input", 1)] defs in
  let prelude = "  section .text\n" ^
                "  extern error\n" ^
                "  extern print\n" ^
                "  global our_code_starts_here\n" in
  let kickoff = "our_code_starts_here:\n" ^
                "push rbx\n" ^
                "  mov [rsp - 8], rdi\n" ^  (* put input in [rsp-8] one on top of stack pointer rsp *)
                to_asm compiled_main ^
                "\n  pop rbx\nret\n" in
  let postlude = [
                  ILabel("overflow_occured");
                  IPush(Const(0));
                  ICall("error")
                 ]
 in
  let as_assembly_string = (to_asm (compiled_defs @ postlude)) in
  sprintf "%s%s\n%s\n" prelude as_assembly_string kickoff
