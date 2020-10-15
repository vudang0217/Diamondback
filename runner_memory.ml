open Unix
open Filename
open Str
open Printf
open OUnit2
open ExtLib
open Sys

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let either_printer e =
  match e with
    | Left(v) -> sprintf "Error: %s\n" v
    | Right(v) -> v

let remove_trailing_newline s =
  let n = String.length s in
  if n > 0 && s.[n-1] = '\n' then
    String.sub s 0 (n-1)
  else
    s

let make_tmpfiles name =
  let (null_stdin, _) = pipe() in
  let stdout_name = (temp_file ("stdout_" ^ name) ".out") in
  let stdin_name = (temp_file ("stderr_" ^ name) ".err") in
  (openfile stdout_name [O_RDWR] 0o600, stdout_name,
   openfile stdin_name [O_RDWR] 0o600, stdin_name,
   null_stdin)

(* Read a file into a string *)
let string_of_file file_name =
  let inchan = open_in file_name in
  let buf = Bytes.create (in_channel_length inchan) in
  really_input inchan buf 0 (in_channel_length inchan);
  Bytes.to_string buf

type result = (string, string) either

let run_no_vg (args : string list) (program_name : string) : result =
  let (rstdout, rstdout_name, rstderr, rstderr_name, rstdin) = make_tmpfiles "run" in
  let ran_pid = Unix.create_process (program_name ^ ".run") (Array.of_list (""::args)) rstdin rstdout rstderr in
  let (_, status) = waitpid [] ran_pid in
  let result = match status with
    | WEXITED 0 -> Right(string_of_file rstdout_name)
    | WEXITED n -> Left(sprintf "Error %d while running: %s" n (string_of_file rstderr_name))
    | WSIGNALED n ->
      Left(sprintf "Signalled with %d while running %s." n program_name)
    | WSTOPPED n ->
      Left(sprintf "Stopped with signal %d while running %s." n program_name) in
  List.iter close [rstdout; rstderr; rstdin];
  List.iter unlink [rstdout_name; rstderr_name];
  result

let run program_string (compiler: string) (out : string) (runner : string -> result) : result =
    let saveSrcfile = open_out ( "input/" ^ out ^ ".boa") in
    fprintf saveSrcfile "%s" program_string;
    close_out saveSrcfile;
    let (bstdout, bstdout_name, bstderr, bstderr_name, bstdin) = make_tmpfiles "build" in
    let built_pid = Unix.create_process "make" (Array.of_list [""; "output/" ^  out ^ ".run"; "COMPILER=" ^ compiler ]) bstdin bstdout bstderr in
    let (_, status) = waitpid [] built_pid in

    let try_running = match status with
    | WEXITED 0 ->
      Right(string_of_file bstdout_name)
    | WEXITED _ ->
      Left(sprintf "Finished with error while building %s:\n%s" out (string_of_file bstderr_name))
    | WSIGNALED n ->
      Left(sprintf "Signalled with %d while building %s." n out)
    | WSTOPPED n ->
      Left(sprintf "Stopped with signal %d while building %s." n out) in

    let result = match try_running with
    | Left(_) -> try_running
    | Right(_) ->
      runner ("output/" ^ out) in

    List.iter close [bstdout; bstderr; bstdin];
    List.iter unlink [bstdout_name; bstderr_name];
    result

let test_run program_str outfile expected args _ =
  let result = run program_str "main" outfile (run_no_vg args) in
    assert_equal (Right(expected ^ "\n")) result ~printer:either_printer

let is_running_error message = String.exists message "while running"

let detect_buggy program_str outfile buggyCompiler args _ =
  let buggyCompilerName = "buggy" ^ (string_of_int buggyCompiler) in
  let _ = if not (Sys.file_exists ("compilers/" ^ buggyCompilerName)) then 
          assert_failure (buggyCompilerName ^ " does not exist") in
  let _ = if not (Sys.file_exists ("compilers/correct")) then 
          assert_failure ("correct compiler does not exist") in
  let result = run program_str buggyCompilerName outfile (run_no_vg args) in
  let expected = run program_str "correct" outfile (run_no_vg args) in
     let (detected, expect_str, actual_str) = 
        match expected, result with
        | Right(expect_val), Right(actual_val) ->
          (expect_val <> actual_val, expect_val, actual_val)
        | Left(expect_msg), Left(actual_msg) ->
          if (is_running_error expect_msg) && (is_running_error actual_msg) then  
             (expect_msg <> actual_msg, expect_msg, actual_msg)
          else
             (false, expect_msg, actual_msg)
        | Left(expect_msg), Right(actual_val) ->
          if (is_running_error expect_msg) then
             (true, expect_msg, actual_val)
          else 
             (false, expect_msg, actual_val)
        | Right(expect_val), Left(actual_msg) ->
          if (is_running_error actual_msg) then 
             (true, expect_val, actual_msg)
          else 
             (false, expect_val, actual_msg)
     in let msg = ("\nCorrect compiler got: " ^ expect_str ^ 
          "\n" ^ buggyCompilerName ^ " got: " ^ actual_str) 
     in if detected then
        let _ = printf "\n==============================================================================\
		\nTest: %s \n\n%s Detected\n %s\n" outfile  buggyCompilerName msg in ()
        else
        assert_failure ("Detecting " ^ buggyCompilerName ^ " failed\n" ^ msg) 


let test_err program_str outfile errmsg args _ =
    let result = run program_str "correct" outfile (run_no_vg args) in
    assert_equal
      (Left(errmsg))
      result
      ~printer:either_printer
      ~cmp: (fun check result ->
        match check, result with
        | Left(expect_msg), Left(actual_message) ->
          String.exists actual_message expect_msg
        | _ -> false
      )

