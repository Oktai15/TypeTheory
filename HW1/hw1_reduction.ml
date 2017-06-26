(* This program allows checking on alpha eqivalent lambda-expressions,                                             *) 
(* check on free substitution lambda-expressions and doing reduction. Note that, it uses Genlex and hw1.ml.        *) 

open Hw1;;

type imperative_lambda = | ImpVar of string 
                         | ImpAbs of (string * imperative_lambda ref)
                         | ImpApp of (imperative_lambda ref * imperative_lambda ref);;

module VarSet = Set.Make(String);;
module NewMap = Map.Make(String);;

let generator_numbers n = Stream.from (fun i -> if i < n then Some i else None);;
let gen_int = generator_numbers 2147000000;;
let next_var() = match (Stream.next gen_int) with
    | k -> "temp" ^ string_of_int k;;

let rec subst_var nv lmd ov = 
    match (lmd) with 
    | Var(v) -> if v = ov then Var(nv) else Var(v);
    | App(l1, l2) -> App(subst_var nv l1 ov, subst_var nv l2 ov);
    | Abs(v, l) -> if v = ov then Abs(nv, subst_var nv l ov) else Abs(v, subst_var nv l ov);;

let rec find_free_var lmd st = 
    match (lmd) with 
    | Var(v) -> VarSet.add v st;
    | App(l1, l2) -> VarSet.union (find_free_var l1 st) (find_free_var l2 st);
    | Abs(v, l) -> VarSet.remove v (find_free_var l st);;

let rec subst_lmd n m x vset = 
	match (m) with 
    | Var(v) -> if (v = x) then n else Var(v);
    | App(l1, l2) -> App(subst_lmd n l1 x vset, subst_lmd n l2 x vset);
    | Abs(v, l) -> if (v = x) then Abs(v, l) else (if (VarSet.mem v vset) then failwith (v ^ " in FV(n)!") else (Abs(v, subst_lmd n l x vset)));;

let rec imperative_lambda_of_lambda lmd =
    match lmd with
    | Var(v) -> ref (ImpVar(v))
    | App(l1, l2) -> ref (ImpApp(imperative_lambda_of_lambda l1, imperative_lambda_of_lambda l2))
    | Abs(v, l) -> ref (ImpAbs(v, imperative_lambda_of_lambda l));;

let rec lambda_of_imperative_lambda imp_lmd = 
    match !imp_lmd with
    | ImpVar(v) -> Var(v)
    | ImpApp(l1, l2) -> App(lambda_of_imperative_lambda l1, lambda_of_imperative_lambda l2)
    | ImpAbs(v, l) -> Abs(v, lambda_of_imperative_lambda l);;
 
let rec subst_imp_lmd n m x = 
    match !m with
    | ImpVar(v) -> if v = x then m := !n
    | ImpApp(l1, l2) -> subst_imp_lmd n l1 x; subst_imp_lmd n l2 x
    | ImpAbs(v, l) -> if v <> x then subst_imp_lmd n l x;;

let rec cmn_tmpl lmd map = 
    match lmd with
	| Var(v) -> if NewMap.mem v map then Var(NewMap.find v map) else lmd
	| App(l1, l2) -> App(cmn_tmpl l1 map, cmn_tmpl l2 map)
	| Abs(v, l) -> let nv = next_var() in
					 Abs(nv, cmn_tmpl l (NewMap.add v nv map));;	

(* Major realiations: free_to_subst, free_vars, is_normal_form,          *)
(* normal_beta_reduction, reduce_to_normal_form, is_alpha_equivalent     *)

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

let rec is_normal_form lmd =
    match lmd with 
    | Var(v) -> true
    | Abs(v, l) -> is_normal_form l
    | App(l1, l2) -> 
        match l1 with 
        | Abs(_, _) -> false
        | _ -> is_normal_form l1 && is_normal_form l2;;		

let normal_beta_reduction lmd =
	let rec impl lmd = 
	  match lmd with
	  | Var(v) -> (false, lmd)
	  | Abs(v, l) -> let (change, nb) = impl l in (change, Abs(v, nb))
      | App(l1, l2) -> 
        match l1 with 
        | Abs(v, l3) -> (true, subst_lmd l2 l3 v (find_free_var l2 (VarSet.empty)))
        | _ -> let (change, na) = impl l1 in
					if change then (change, App(na, l2))
					else let (change, nb) = impl l2 in
						(change, App(l1, nb))
	in
	let (_, blmd) = impl (cmn_tmpl lmd NewMap.empty) in
	blmd;;

let rec reduce_to_normal_form lmd = 
	let imp_lmd = imperative_lambda_of_lambda (cmn_tmpl lmd NewMap.empty) in
	let rec impl imp_lmd = match !imp_lmd with
	  | ImpVar(v) -> None
	  | ImpApp(l1, l2) ->
		(match !l1 with					
		| ImpAbs(a, b) -> let nv = next_var() in
						    imp_lmd := !(imperative_lambda_of_lambda(cmn_tmpl (lambda_of_imperative_lambda b) (NewMap.singleton a nv)));
							subst_imp_lmd l2 imp_lmd nv;
							Some imp_lmd
		| _ -> match impl l1 with
			    | Some blmd -> Some imp_lmd
				| None -> match impl l2 with
								| Some blmd -> Some imp_lmd
								| None -> None)				
	  | ImpAbs(a, b) -> match impl b with
						        | Some blmd -> Some imp_lmd
						        | None -> None					
	in 
	let rec ret_impl imp_lmd = match impl imp_lmd with
	  | Some blmd -> ret_impl blmd
	  |	None -> imp_lmd
	in
	lambda_of_imperative_lambda (ret_impl imp_lmd);;