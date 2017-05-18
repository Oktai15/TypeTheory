(* This program does unification algorithm.   *) 

type algebraic_term = Var of string | Fun of string * (algebraic_term list)
type substitution = (string * algebraic_term) list
type equation = algebraic_term * algebraic_term
type system_of_equations = equation list 

let generator_numbers n = Stream.from (fun i -> if i < n then Some i else None);;
let gen10000 = generator_numbers 10000;;

let next_name_fun() = match (Stream.next gen10000) with
    | k -> "name" ^ string_of_int k;;

let rec algebraic_term_to_string (at : algebraic_term) = 
    let rec impl a =
        match a with 
        | Var x -> x
        | Fun(f, l) -> f ^ "(" ^ impl_for_list l ^ ")" 
    
    and impl_for_list lt = 
        match lt with 
        | [] -> ""
        | (h::[]) -> impl h
        | (h::t) -> (impl h) ^ " " ^ (impl_for_list t)
    in
    impl at;;

let rec is_equal_algebraic_term (fat : algebraic_term) (sat : algebraic_term) : bool = 
    match (fat,sat) with 
    | (Var(x), Var(y)) -> if x = y then true else false
    | (Fun(f1, l1), Fun(f2, l2)) -> if f1 = f2 then 
        List.for_all (fun e -> is_equal_algebraic_term (fst e) (snd e)) (List.combine l1 l2) else false;
    | _ -> false;;

let rec is_contains (x : string) (t : algebraic_term) : bool =
	match t with
	| Var y -> x = y
	| Fun(_, s) -> List.exists (is_contains x) s

let rec subst (s : algebraic_term) (x : string) (t : algebraic_term) : algebraic_term =
	match t with 
	| Var y -> if (x = y) then s else t
	| Fun(f, l) -> Fun(f, List.map (subst s x) l);;
    

(* Major implementations: system_to_equation, apply_substitution,    *)
(* check_solution, solve_system.                                 *)

let system_to_equation (stm : system_of_equations) : equation =
    let new_name = next_name_fun() in
    let two_columns = List.split stm in 
    Fun(new_name, fst two_columns), Fun(new_name, snd two_columns);;    

let apply_substitution (s : substitution) (at : algebraic_term) : algebraic_term =
    List.fold_right (fun si u -> subst (snd si) (fst si) u) s at;;

let check_solution (s : substitution) (stm : system_of_equations) : bool = 
	let eq = system_to_equation stm in
	    is_equal_algebraic_term (apply_substitution s (fst eq)) (apply_substitution s (snd eq));;

let rec solve_system (sym : system_of_equations) : substitution option =
    let unify_eq (s : algebraic_term) (t : algebraic_term) : substitution = 
        match (s, t) with 
        | (Var x, Var y) -> if (x = y) then [] else [(x, t)]
        | (Fun (f, l1), Fun(g, l2)) -> 
            if (f = g) && List.length l1 = List.length l2
            then let sm = solve_system (List.combine l1 l2) in
                match sm with
                | Some vr -> vr 
                | None -> failwith "It won't be!" 
            else failwith "Not the same length!" 
        | ((Var x, (Fun(_, _) as term))  | ((Fun(_, _) as term), Var x)) ->
            if is_contains x term then failwith "The same variable!" else [(x, term)]
    in
    try 
        match sym with 
        | [] -> Some ([])
        | (x, y) :: t -> 
            let term2 = solve_system t in
            match term2 with
            | Some k -> Some((unify_eq (apply_substitution k x) (apply_substitution k y)) @ k)   
            | None -> None
    with 
    | _ -> None