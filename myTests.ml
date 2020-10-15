open Runner
open Expr
open Printf
open OUnit2
open Parser  

(* Fill in `myTestList` with your own tests. There are two ways to add a test:
 *
 * (1) By adding the code of the program as a string. In this case, add an entry
 *     of this form in `myTestList`:
 *
 *     t <test_name> <program_code> <result>
 *
 * (2) By adding a test inside the 'input/' folder and adding an entry for it
 *     in `myTestList`. The entry in this case should be:
 *
 *     t_file <test_name> <file_name> <result>
 *     
 *     Where name is the name of the file inside 'input/' with the extension
 *     ".ana". For example:
 *
 *     t_file "myTest" "mytest.ana" "6";
 *)

let t_i name program expected args = name>::test_run program name expected args
let t name program expected = name>::test_run program name expected []
let terr_i name program expected args = name>::test_err program name expected args
let t_err name program expected = name>::test_err program name expected []
let t_parse name program expected =
  name>::(fun _ -> assert_equal expected (Runner.parse_string program));;

let dup_def = "(def f (x : Num) : Num x) (def f (x : Num y : Num) : Num (+ x y))"
let multi_args_func = "(def f (x : Num y : Num x : Num y : Num) : Num (* x y))"
let good_def1 = "(def f (x : Num) : Num x)" 
let good_def2 = "(def g (x : Num y : Num z : Bool) : Num (if z x y))"
let good_def3 = "(def j () : Bool false)"
let good_def4 = "(def plus (x : Num y : Num) : Num (+ x y))"
let bad_eapp_num_args = "(f 1 2)"
let bad_eapp_mismatch_type = "(g 1 true true)"
let bad_eapp_unbound = "(h 1 2 3 4 5)"
let good_eapp = "(g (+ 1 2) (+ 2 3) (< 1 2))"
let good_eapp1 = "(j)"
let print_def = "(def g (x : Num y : Num z : Bool) : Num (print x) (print y) (print (if z 3 4)) (let ((x 3)) (print (+ y x))))"

let myTestList =
  [ (* Fill in your tests here: *)
    t_parse "parse_app" "(f 1 2)" (EApp("f",[ENumber(1);ENumber(2)]));
    t_parse "parse_app1" "(f)" (EApp("f",[]));
    t_err "well_form_err" (dup_def^"(f 2)") "Multiple functions";
    t_err "well_form_err2" (multi_args_func^"(f 1 2 3)") "Multiple bindings";
    t_err "tc_num_args" (good_def1^good_def2^bad_eapp_num_args) "Type mismatch : EApp, length args not match";
    t_err "tc_mismatch_type" (good_def1^good_def2^bad_eapp_mismatch_type) "Type mismatch : EApp, args type not  match";
    t_err "tc_unbound_func" (good_def1^good_def2^bad_eapp_unbound) "Unbound function h";
    t "tc_good1" (good_def1^good_def2^good_eapp) "3";
    t "tc_good2" (good_def1^good_def2^good_def3^good_eapp1) "false";
    t "tc_good3" (print_def^good_def1^good_def3^good_def4^"(g (f (+ 3 4)) (plus 4 5) (j))") "7\n9\n4\n12\n12" 

  ]
;;
