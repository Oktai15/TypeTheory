(* This program allows parsing lambda-expressions, checking on alpha eqivalent lambda-expressions *) 
(*and check on free substitution lambda-expressions. Note that, it uses Genlex.                   *) 

(* Write in console to run:                        *)
(* > ocamlc -o <EXECUTOR-NAME> hw2.mli hw2.ml t.ml *)
(* > ./<EXECUTOR-NAME>                             *)

open Genlex;;

(* Definition types for lambda-expressions  *)

type lambda = 
		| Var of string
		| Abs of string * lambda
		| App of lambda * lambda;;

(* Functions for parser lambda-expressions:                   *)
(* string_of_lambda, parse_lambda_of_tokens, lambda_of_string *) 

let end_sym = ";";;
let end_token_sym = Kwd ";";; (* No use *)
let lexer_lambda = make_lexer ["\\";".";"(";")";";"];;
let str_with_end_sym str = str^end_sym;;

let rec string_of_lambda x =
    match x with
    | Var l1 -> l1
    | App (a,b) -> "(" ^ string_of_lambda a ^ " " ^ string_of_lambda b ^ ")"
    | Abs (s,e) -> "(\\" ^ s ^ "." ^ string_of_lambda e ^ ")";; 

let parse_lambda_of_tokens str_tokens =
    let tn() = Stream.next str_tokens in
    let pk() = Stream.peek str_tokens in
    let cb() = if (tn() <> Kwd ")") then failwith "Unexpected symbol" in
    let comm() = if (tn() <> Kwd ".") then failwith "Unexpected symbol" in
    
    let rec parse_lambda() =
	match (tn()) with
        | Kwd "("  -> 
                let pl = parse_lambda() in
            	cb();
                maybe_app pl;

        | Kwd "\\" ->
                let pa = parse_abs() in
     		maybe_app pa;

        | Ident s  ->
                let v = Var s in
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
        | None   -> failwith "Unexpected error";
        | Some k -> 
                (match k with 
                 | Kwd ")" -> l_app
      		     | Kwd ";" -> l_app
        	     | _ -> App(l_app, parse_lambda())) in

    parse_lambda();; 
 
let lambda_of_string x = parse_lambda_of_tokens (lexer_lambda(Stream.of_string (str_with_end_sym x)));;

let generator_numbers n = Stream.from (fun i -> if i < n then Some i else None);;

let gen1000 = generator_numbers 1000;;

let next_var() = match (Stream.next gen1000) with
    | k -> "t" ^ string_of_int k;;

let rec subst nv lmd ov = 
    match (lmd) with 
    | Var(v) -> if v = ov then Var(nv) else Var(v);
    | App(l1, l2) -> App(subst nv l1 ov, subst nv l2 ov);
    | Abs(v, l) -> if v = ov then Abs(nv, subst nv l ov) else Abs(v, subst nv l ov);;

let rec is_alpha_equivalent lmd1 lmd2 =
    match (lmd1, lmd2) with 
    | (Var(v1), Var(v2)) -> if v1 = v2 then true else false;
    | (App(fl1, fl2), App(sl1, sl2)) -> 
            if (is_alpha_equivalent fl1 sl1 && is_alpha_equivalent fl2 sl2) 
            then true else false;
    | (Abs(v1, l1), Abs(v2, l2)) -> 
            let nv = next_var() in
            is_alpha_equivalent (subst nv l1 v1) (subst nv l2 v2); 
    | _ -> false;;



