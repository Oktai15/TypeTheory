open Hw2;;


(* Testing is_alpha_equivalent *)

let t1 = (lambda_of_string "(x)", lambda_of_string "(y)");;
let t2 = (lambda_of_string "x y", lambda_of_string "x y");;
let t3 = (lambda_of_string "\\x.x y", lambda_of_string "\\y.y y");;
let t4 = (lambda_of_string "\\x1.x1", lambda_of_string "\\y1.y1");;
let t5 = (lambda_of_string "(\\x.x)(z)(w)", lambda_of_string "(\\y.y)(z)(w)");;
let t6 = (lambda_of_string "\\x1.\\x2.\\x3.\\x4.x1 x2 x3 x4", lambda_of_string "\\y1.\\y2.\\y3.\\y4.y1 y2 y3 y4");;
let t7 = (lambda_of_string "\\x1.\\x2.\\x3.\\x4.x4 x2 x3 x1", lambda_of_string "\\y1.\\y2.\\y3.\\y4.y1 y2 y3 y4");;
let t8 = (lambda_of_string "\\x1.\\x2.x1 x2", lambda_of_string "\\y1.\\y2.y2 y1");;

let tester_on_alpha_eq pair = 
    print_string(string_of_bool(is_alpha_equivalent (fst pair) (snd pair)));
    print_newline();;   

tester_on_alpha_eq t1;;
tester_on_alpha_eq t2;;
tester_on_alpha_eq t3;;
tester_on_alpha_eq t4;;
tester_on_alpha_eq t5;;
tester_on_alpha_eq t6;;
tester_on_alpha_eq t7;;
tester_on_alpha_eq t8;;
