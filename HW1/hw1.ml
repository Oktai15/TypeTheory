(* ocamlc -o peano hw1.mli hw1.ml t.ml *)

type peano = Z | S of peano;;

type lambda = 
		| Var of string
		| Abs of string * lambda
		| App of lambda * lambda;;

let rec int_of_peano x = match x with 
	| Z -> 0
	| S a -> 1 + int_of_peano a;;

let rec peano_of_int x = match x with 
	| 0 -> Z
	| x -> S (peano_of_int (x-1));;

let inc x = S x;;

let rec add x y = match (x,y) with 
	| (x, Z) -> x
	| (Z, y) -> y
	| (x, S b) -> S (add x b);;

let rec mul x y = match (x,y) with 
	| (x, Z) -> Z
	| (Z, y) -> Z
	| (x, S b) -> add x (mul b x);;

let rec sub x y = match (x,y) with 
	| (Z, y) -> Z
	| (x, Z) -> x
	| (S a, S b) -> sub a b;;


let rec power x y = match (x,y) with 
	| (x, Z) -> S Z 
	| (Z, y) -> Z
	| (x, S b) -> mul x (power x b);;
                     
let rec dop_rev x y = match (x,y) with 
	| (x, []) -> x
	| (x, h::ys) -> dop_rev (h::x) ys;;

let rev x = match x with 
	| [] -> []
	| x -> dop_rev [] x;; 

let merge_sort x = failwith "Not implemented";;
                     
let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";; 

let rec print_list x = match x with 
	| [] -> print_string " "
	| a::xs -> print_int a; print_string " "; print_list xs;;

(*let main() = 
	let arg1 = peano_of_int (int_of_string Sys.argv.(1)) in
	let arg2 = peano_of_int (int_of_string Sys.argv.(2)) in
	print_int (int_of_peano (sub arg1 arg2));
	print_newline();
	exit 0;;
main();;*)
	


