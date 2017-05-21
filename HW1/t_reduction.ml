open Hw1_reduction;;
open Hw1;;

(* Write in console to run:                        *)
(* > ocamlc -o <EXECUTOR-NAME> hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml t_reduction.ml *)
(* > ./<EXECUTOR-NAME>                             *)

type test_for_alpha_eq = {lmd1 : lambda; lmd2 : lambda; ans : bool};;
type test_for_free_subst = {n : lambda; m : lambda; x : string; ans : bool};;
type test_free_vars = {lmd : lambda; list_ans : string list};;
type test_reduction = {lmd_r : lambda; rdt : lambda};;

let tests_fae = 
    [{lmd1 = lambda_of_string "(x)"; lmd2 = lambda_of_string "(y)"; ans = false};
    {lmd1 = lambda_of_string "x y"; lmd2 = lambda_of_string "x y"; ans = true};
    {lmd1 = lambda_of_string "\\x.x y"; lmd2 = lambda_of_string "\\y.y y"; ans = false};
    {lmd1 = lambda_of_string "\\x1.x1"; lmd2 = lambda_of_string "\\y1.y1"; ans = true};
    {lmd1 = lambda_of_string "(\\x.x)(z)(w)"; lmd2 = lambda_of_string "(\\y.y)(z)(w)"; ans = true};
    {lmd1 = lambda_of_string "\\x1.\\x2.\\x3.\\x4.x1 x2 x3 x4";
         lmd2 = lambda_of_string "\\y1.\\y2.\\y3.\\y4.y1 y2 y3 y4"; ans = true};
    {lmd1 = lambda_of_string "\\x1.\\x2.\\x3.\\x4.x4 x2 x3 x1";
         lmd2 = lambda_of_string "\\y1.\\y2.\\y3.\\y4.y1 y2 y3 y4"; ans = false};
    {lmd1 = lambda_of_string "\\x1.\\x2.x1 x2";  lmd2 = lambda_of_string "\\y1.\\y2.y2 y1"; ans = false}];;

let tests_ofs = 
    [{n = lambda_of_string "x"; m = lambda_of_string "\\x.y"; x = "y"; ans = false};
    {n = lambda_of_string "x"; m = lambda_of_string "\\x.x"; x = "y"; ans = false}; (*!!!!*)
    {n = lambda_of_string "x"; m = lambda_of_string "(x) (\\x.y)"; x = "y"; ans = false};
    {n = lambda_of_string "x y \\z.z"; m = lambda_of_string "\\x.a"; x = "a"; ans = false};
    {n = lambda_of_string "x y \\z.z"; m = lambda_of_string "\\y.a"; x = "a"; ans = false};
    {n = lambda_of_string "x y \\z.z"; m = lambda_of_string "\\z.a"; x = "a"; ans = true};
    {n = lambda_of_string "x y \\z.z"; m = lambda_of_string "a (\\z.a)"; x = "a"; ans = true};
    {n = lambda_of_string "x y \\z.z"; m = lambda_of_string "\\x.b"; x = "a"; ans = false}];; (*!!!*)

let tests_fv = 
    [{lmd = lambda_of_string "x"; list_ans = ["x"]};
    {lmd = lambda_of_string "\\x.x"; list_ans = []}; 
    {lmd = lambda_of_string "(x) (\\x.y)"; list_ans = ["x";"y"]}];;

let tests_nbr = 
    [{lmd_r = lambda_of_string "(\\x.x) a"; rdt = lambda_of_string "a"};
    {lmd_r = lambda_of_string "a ((\\y.\\z.y) (\\p.p))"; rdt = lambda_of_string "a \\z.\\p.p"};
    {lmd_r = lambda_of_string "(\\x.x) (\\y.y) (\\z.z))"; rdt = lambda_of_string "((\\y.y) (\\z.z))"};
    {lmd_r = lambda_of_string "\\z.((\\x.x) y)"; rdt = lambda_of_string "\\z.y"}];;

let tests_rnf = 
    [{lmd_r = lambda_of_string "(\\x.\\y.y)((\\z.z z)(\\z.z z))"; rdt = lambda_of_string "\\y.y"};
    {lmd_r = lambda_of_string "a ((\\y.\\z.y) (\\p.p))"; rdt = lambda_of_string "a \\z.\\p.p"};
    {lmd_r = lambda_of_string "(\\x.x) (\\y.y) (\\z.z))"; rdt = lambda_of_string "(\\z.z)"};
    {lmd_r = lambda_of_string "(\\x.x x)(\\a.\\b.b b b)"; rdt = lambda_of_string "\\b.b b b"};
    {lmd_r = lambda_of_string "(\\x.x x x)((\\x.x)(\\x.x))"; rdt = lambda_of_string "\\x.x"}];;

let tester_on_alpha_eq t = if (is_alpha_equivalent (t.lmd1) (t.lmd2) = t.ans) then true else false;;
let tester_on_free_subst t = if (free_to_subst (t.n) (t.m) (t.x) = t.ans) then true else false;;
let tester_on_free_vars t = if (free_vars t.lmd = t.list_ans) then true else false;;
let tester_on_normal_beta_reduction t = 
    let k = normal_beta_reduction t.lmd_r in
    match (k = t.rdt) with
    | true  -> true;
    | false -> print_string(string_of_lambda k ^ "\n"); false;;

let tester_on_reduce_to_normal_form t = 
    let k = reduce_to_normal_form t.lmd_r in
    match (k = t.rdt) with
    | true  -> true;
    | false -> print_string(string_of_lambda k ^ "\n"); false;;


let rec tester pred name l ind cor incor = 
	match l with 
	| [] ->  print_string ("Testing on <" ^ name ^ "> has done!\nYour stats: " ^ "correct answers: " ^ (string_of_int cor) ^
	                                                          ", incorrect answers: " ^ (string_of_int incor)); print_newline(); print_newline();
    | x::xs -> if ((pred x) = true) then
                    (print_string ("Test#" ^ (string_of_int ind) ^ ": correct :)\n");
                    tester pred name xs (ind + 1) (cor + 1) incor)
                    else
                    (print_string ("Test#" ^ (string_of_int ind) ^ ": incorrect :(\n");
                    tester pred name xs (ind + 1) cor (incor + 1));;


tester (tester_on_alpha_eq) "is_alpha_equivalent" tests_fae 1 0 0;;
tester (tester_on_free_subst) "free_subst" tests_ofs 1 0 0;;
tester (tester_on_free_vars) "free_vars" tests_fv 1 0 0;;
tester (tester_on_normal_beta_reduction) "normal_beta_reduction" tests_nbr 1 0 0;;
tester (tester_on_reduce_to_normal_form) "reduce_to_normal_form" tests_rnf 1 0 0;;