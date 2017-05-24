(* This program allows checking on alpha eqivalent lambda-expressions,                                             *) 
(* check on free substitution lambda-expressions and doing reduction. Note that, it uses Genlex and hw1.ml.        *) 
open Hw1;;

module HMapLmd = Hashtbl.Make(struct 
    type t = lambda
    let rec equal lmd1 lmd2 = 
        match (lmd1, lmd2) with 
    | (Var(v1), Var(v2)) -> if v1 = v2 then true else false;
    | (App(fl1, fl2), App(sl1, sl2)) -> 
            if (equal fl1 sl1 && equal fl2 sl2) 
            then true else false;
    | (Abs(v1, l1), Abs(v2, l2)) -> 
            if v1 = v2 then equal l1 l2 else false; 
    | _ -> false;;
    let hash n = Hashtbl.hash n
end)

module VarSet = Set.Make (String);;

let generator_numbers n = Stream.from (fun i -> if i < n then Some i else None);;
let gen1000 = generator_numbers 1000;;
let next_var() = match (Stream.next gen1000) with
    | k -> "t" ^ string_of_int k;;

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
    | Abs(v, l) -> if (v = x) then Abs(v, l) else (if (VarSet.mem v vset) then (failwith "v in FV(n)!") else (Abs(v, subst_lmd n l x vset)));;

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
    | Var x -> true
    | Abs(a, b) -> is_normal_form b
    | App(a, b) -> 
        match a with 
        | Abs(_, _) -> false
        | _ -> is_normal_form a && is_normal_form b;;

let rec normal_beta_reduction lmd = 
    if (is_normal_form lmd) then lmd else 
        match lmd with
        | Var x -> lmd
        | Abs(a, b) -> Abs(a, normal_beta_reduction b)
        | App(a, b) -> 
            match a with 
            | Abs(x, y) -> subst_lmd b y x (find_free_var b (VarSet.empty));
            | _ ->  if is_normal_form a then
                        App(a, (normal_beta_reduction b))
                    else
                        App((normal_beta_reduction a), b);;

let rec reduce_with_mem lmd hmap =
    print_string((string_of_lambda lmd) ^ "\n");
    let k = is_normal_form lmd in 
    match k with
    | false -> if (HMapLmd.mem hmap lmd) then
                (print_string ((string_of_lambda lmd) ^ " has already had\n");
                HMapLmd.find hmap lmd)
                else 
                let a = one_reduction_mem lmd hmap in
                    if is_normal_form a then
                        (HMapLmd.add hmap lmd a; a)
                    else reduce_with_mem a hmap
    | true  -> lmd
and one_reduction_mem lmd hmap =  
    if (is_normal_form lmd) then lmd else
        match lmd with
        | Var x -> lmd
        | Abs(a, b) -> Abs(a, reduce_with_mem b hmap)
        | App(a, b) -> 
            (match a with 
            | Abs(x, y) -> subst_lmd b y x (find_free_var b (VarSet.empty));
                (*(match b with
                | App(l, r) -> App(subst_lmd l y x (find_free_var l (VarSet.empty)), r)
                | _ -> subst_lmd b y x (find_free_var b (VarSet.empty)))*)
            | _ ->  if is_normal_form a then
                        App(a, reduce_with_mem b hmap)
                    else
                        App(reduce_with_mem a hmap, b));;

let hm = HMapLmd.create(1000);; 
let reduce_to_normal_form lmd = reduce_with_mem lmd hm;;