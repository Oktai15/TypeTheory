(*open Genlex*)
(* ocamlc -o peano hw1.mli hw1.ml t.ml *)
open Genlex;;

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

let rec merge xs_1 xs_2 =
	match (xs_1, xs_2) with 
	| ([], xs_2) -> xs_2
	| (xs_1, []) -> xs_1
	| (x::s_1, y::s_2) ->
	         if x>y then y::(merge xs_1 s_2) else x::(merge s_1 xs_2);;

let rec split_at mid l = 
	match mid with 
	| 0 -> ([], l)
	| _ -> match l with
		   | [] -> failwith "How?!" (*NEVER*) 
		   | (x::xs) -> let res = split_at (mid-1) xs in 
		   match res with 
		   | (lr, rr) -> (x::lr, rr);;

let rec merge_sort x = 
	let
	  sz = List.length x
	in 
	let 
	  halves = split_at (sz/2) x
	in
	match halves with
	| _ when (sz = 1 || sz = 0) -> x
	| (half_l, half_r) -> merge (merge_sort half_l) (merge_sort half_r);;
                     
let rec print_int_list x = match x with 
	| [] -> print_string " "
	| a::xs -> print_int a; print_string " "; print_int_list xs;;

let end_sym = ";";;
let end_token_sym = Kwd ";";;

let rec string_of_lambda x =
 match x with
	| Var l1 -> l1
	| App (a,b) -> "(" ^ string_of_lambda a ^ " " ^ string_of_lambda b ^ ")"
	| Abs (s,e) -> "(\\" ^ s ^ "." ^ string_of_lambda e ^ ")";; 


let lexer_lambda = make_lexer ["\\";".";"(";")";";"];;

let parse_lambda_of_tokens str_tokens =
	let
	  tn() = Stream.next str_tokens
    in
    let
	  pk() = Stream.peek str_tokens
    in
    let 
      cb() = if (tn() <> Kwd ")") then failwith "Unexpected symbol" 
	in
	let 
      comm() = if (tn() <> Kwd ".") then failwith "Unexpected symbol" 
	in
	let rec parse_lambda() =
		match (tn()) with
            | Kwd "("  -> let pl = parse_lambda() in
            	cb();
                maybe_app pl;

            | Kwd "\\" -> let pa = parse_abs() in
     			maybe_app pa;

            | Ident s  -> let v = Var s in
     			maybe_app v;

            | _ -> failwith "Unexpected symbol"

    and parse_abs() = 
        match (tn()) with 
            | Ident s -> 
         		comm();
    			Abs(s, parse_lambda());
    		| _ -> failwith "Unexpected symbol" 

    and maybe_app l_app = 
        match (pk()) with 
        | None -> failwith "Unexpected error";
        | Some k -> match k with 
        				| Kwd ")" -> l_app
        				| Kwd ";" -> l_app
        				| _ -> App(l_app, parse_lambda()) in

    parse_lambda();; 
let str_with_end_sym str = str^end_sym;; 
let lambda_of_string x = parse_lambda_of_tokens (lexer_lambda(Stream.of_string (str_with_end_sym x)));;