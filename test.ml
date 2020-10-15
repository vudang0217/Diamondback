open Compile
open Runner
open Printf
open OUnit2
open Expr
open ExtLib

let t_i name program expected args = name>::test_run program name expected args
let t name program expected = name>::test_run program name expected []
let terr_i name program expected args = name>::test_err program name expected args
let t_err name program expected = name>::test_err program name expected []
let t_parse name program expected =
  name>::(fun _ -> assert_equal expected (Runner.parse_string program));;
let t_compile_err (name: string) (program: string) (expected: string) =
  name>::(fun _ ->
    (assert_equal expected (try_compile (Runner.parse_string_full program))
       ~printer: (fun s -> s) ~cmp: (fun expected actual -> (String.exists actual expected))))

let num_neg = "(+ -42 10)";;
let forty_one = "(sub1 42)";;
let forty = "(sub1 (sub1 42))";;
let add1 = "(add1 (add1 (add1 3)))";;
let def_x = "(let ((x 5)) x)";;
let def_x2 = "(let ((x 5)) (sub1 x))";;
let def_x3 = "(let ((x 5)) (let ((x 67)) (sub1 x)))";;
let def_x4 = "(let ((x (let ((x 5)) (sub1 x)))) (sub1 x))";;
let addnums = "(+ 5 10)";;
let nested_add = "(+ 5 (+ 10 20))";;
let nested_add2 = "(+ (- 10 5) 20)";;
let nested_arith = "(- (* (- 54 3) 2) 102)";;
let let_nested = "(let ((x (+ 5 (+ 10 20)))) (* x x))"
let complexExpression = "(let ((x 10) (y 5) (z 3)) (let ((t 2)) " ^
                        "(add1 (+ x (+ y (* (- t z) x))))))"
let ifTest = "(if true 5 6)"
let ifTestLet = "(let ((x 5)) (if (== x 7) 7 8))"
let setTest = "(let ((x 1)) (set x 2) x)"
let whileTest = "(let ((c 10) (x 0)) (while (> c x) (set x (+ x 1))) x)"
let boolTest = "true"
let isBoolTest = "(isBool false)"
let isBoolTestF = "(isBool 5)"
let isNumTest = "(isNum 5)"
let defTest = "(def abs_val (x : Num) : Num (if (< x 0) (* -1 x) x)) (abs_val -3)"

let num_p_overflow = "4611686018427387904"
let num_p_underflow = "-4611686018427387905"

let failLet = "(let ((x  1) (y 1) (x 10)) x)"
let failID = "x"

let autograde_compile_fail_tests =
  [
    ("add1_arguments", "(add1 true)", "Type mismatch");
    ("plus_arguments", "(+ 1 true)", "Type mismatch");
    ("if_condition", "(if 1 2 (+ 3 2))", "Type mismatch");
    ("if_branches", "(if true false (+ 3 2))", "Type mismatch");
    ("fun_arguments", "(def mul2 (x : Num) : Num (* x 2)) (mul2 false)", "Type mismatch");
  ]

let testFailList =
  [
   t_err "failLet" failLet "Multiple bindings for variable identifier x";
   t_err "failID" failID "Variable identifier x unbound";
   t_err "parserNumOverflow" num_p_overflow "Non-representable number";
   t_err "parserNumUnderflow" num_p_underflow "Non-representable number";
   terr_i "failInput" "input" "input must be a number" ["0r"];
  ]

let input_tests =
 [ t_i "input1" "input" "42" ["42"]
 ; t_i "input_default" "input" "0" []
 ; t_i "input_shadow" "(let ((input 10)) input)" "10" ["0"]

 ; terr_i "inputerr1" "input" "input must be a number" ["ABC"]
 ; terr_i "inputerr_max" "input" "input is not a representable number" ["4611686018427387904"]
 ; terr_i "inputerr_min" "input" "input is not a representable number" ["-4611686018427387905"]
 ; terr_i "inputerr_case" "input" "input must be a number" ["False"]
 ]

let suite =
  "suite">:::
  
  [t "num_neg" num_neg "-32";
   t "forty_one" forty_one "41";
   t "forty" forty "40";
   t "add1" add1 "6";
   t "def_x" def_x "5";
   t "def_x2" def_x2 "4";
   t "def_x3" def_x3 "66";
   t "def_x4" def_x4 "3";
   t "addnums" addnums "15";
   t "boolTest" boolTest "true";
   t "if_Test" ifTest "5";
   t "ifTestLet" ifTestLet "8";
   t "setTest" setTest "2";
   t "whileTest" whileTest "10";
   t "isBoolTest" isBoolTest "true";
   t "isBoolTestF" isBoolTestF "false";
   t "isNumTest" isNumTest "true";
   t "defTest" defTest "3";
   t_i "inputTest" "(add1 input)" "6" ["5"];
  ] @ testFailList
  @  MyTests.myTestList

let () =
  run_test_tt_main suite
;;
