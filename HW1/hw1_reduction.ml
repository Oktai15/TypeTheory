(* This program allows checking on alpha eqivalent lambda-expressions *) 
(*and check on free substitution lambda-expressions. Note that, it uses Genlex and hw1.ml                   *) 

open Genlex;;
open Hw1;;

(* Functions for check on alpha equivalent lambda expressions and free substation lambda expression instead of variable in lambda-expression:  *)
(* subst_var, is_alpha_equivalent, free_to_subst, find_free_var, subst_lmd                                                                                       *) 

let generator_numbers n = Stream.from (fun i -> if i < n then Some i else None);;
let gen1000 = generator_numbers 1000;;
let next_var() = match (Stream.next gen1000) with
    | k -> "t" ^ string_of_int k;;

(*
let rec subst_var nv lmd ov = 
    match (lmd) with 
    | Var(v) -> if v = ov then Var(nv) else Var(v);
    | App(l1, l2) -> App(subst_var nv l1 ov, subst_var nv l2 ov);
    | Abs(v, l) -> if v = ov then Abs(nv, subst_var nv l ov) else Abs(v, subst_var nv l ov);;

let rec is_alpha_equivalent lmd1 lmd2 =
    match (lmd1, lmd2) with 
    | (Var(v1), Var(v2)) -> if v1 = v2 then true else false;
    | (App(fl1, fl2), App(sl1, sl2)) -> 
            if (is_alpha_equivalent fl1 sl1 && is_alpha_equivalent fl2 sl2) 
            then true else false;
    | (Abs(v1, l1), Abs(v2, l2)) -> 
            let nv = next_var() in
            is_alpha_equivalent (subst_var nv l1 v1) (subst_var nv l2 v2); 
    | _ -> false;;
*)
module VarSet = Set.Make (String);;

let rec find_free_var lmd st = 
    match (lmd) with 
    | Var(v) -> VarSet.add v st;
    | App(l1, l2) -> VarSet.union (find_free_var l1 st) (find_free_var l2 st);
    | Abs(v, l) -> VarSet.remove v (find_free_var l st);;

let rec subst_lmd n m x vset = 
	match (m) with 
    | Var(v) -> if (v = x) then n else Var(v);
    | App(l1, l2) -> App(subst_lmd n l1 x vset, subst_lmd n l2 x vset);
    | Abs(v, l) -> if (v = x) then Abs(v, l) else (if (VarSet.mem v vset) then (failwith "v in FV(n)!") else (Abs(v, subst_lmd n l x vset)));;

(* General realiations *)

let free_to_subst n m x = 
	try
	    let sl = subst_lmd n m x (find_free_var n (VarSet.empty)) in  
	    match sl with 
	    | _ -> true
	with 
	| _ -> false

let free_vars lmd = 
    let vset = find_free_var lmd VarSet.empty in 
	VarSet.elements vset;;


let is_normal_form lmd = failwith "Not implemented";;
let normal_beta_reduction lmd = failwith "Not implemented";;
let reduce_to_normal_form lmd = failwith "Not implemented";;
