(* This program does unification algorithm.   *) 

type algebraic_term = Var of string | Fun of string * (algebraic_term list)
type substitution = (string * algebraic_term) list
type equation = algebraic_term * algebraic_term
type system_of_equations = equation list 

let generator_numbers n = Stream.from (fun i -> if i < n then Some i else None);;
let gen10000 = generator_numbers 10000;;

let next_name_fun() = match (Stream.next gen10000) with
    | k -> "name" ^ string_of_int k;;

exception NoSolution of string;;
module StringSet = Set.Make (String);;

let rec contains str at msk = 
    match at with
        (Var a) -> if a = str then msk lor 1 else msk 
        | (Fun (f, l)) -> (contains_l str l msk) lor (if str = f then 2 else 0)
and contains_l str l msk =
    match l with
        [] -> msk
        | (h::t) -> (contains str h msk) lor (contains_l str t msk);;         

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

let memv str at =
    contains str at 0 land 1 <> 0;;

let rec is_contains (x : string) (t : algebraic_term) : bool =
	match t with
	| Var y -> x = y
	| Fun(_, s) -> List.exists (is_contains x) s

let rec subst (s : algebraic_term) (x : string) (t : algebraic_term) : algebraic_term =
	match t with 
	| Var y -> if (x = y) then s else t
	| Fun(f, l) -> Fun(f, List.map (subst s x) l);;   

(* Major implementations: system_to_equation, apply_substitution,    *)
(* check_solution, solve_system.                                     *)

let system_to_equation (stm : system_of_equations) : equation =
    let new_name = next_name_fun() in
    let two_columns = List.split stm in 
    Fun(new_name, fst two_columns), Fun(new_name, snd two_columns);;    

let apply_substitution (s : substitution) (at : algebraic_term) : algebraic_term =
    List.fold_right (fun si u -> subst (snd si) (fst si) u) s at;;

let apply_substitution_sys subst sys =
    let rec impl subst sys acc =
        match sys with
            [] -> List.rev acc
            | ((l, r)::t) -> impl subst t (((apply_substitution subst l), (apply_substitution subst r))::acc) in
    impl subst sys [];; 

let check_solution (s : substitution) (stm : system_of_equations) : bool = 
	let eq = system_to_equation stm in
	    is_equal_algebraic_term (apply_substitution s (fst eq)) (apply_substitution s (snd eq));;

let rec solve_system (sys : system_of_equations) : substitution option =
    let get_args_sys l1 l2 =
        let rec impl l1 l2 ans =
            match (l1, l2) with 
                ([], []) -> List.rev ans 
                | (h1::t1, h2::t2) -> impl t1 t2 ((h1, h2)::ans) 
                | _ -> failwith "never" in
        impl l1 l2 [] in
        
    let rec impl sys resolved =
        if StringSet.cardinal resolved = List.length sys then sys else   
        match sys with
            [] -> raise (NoSolution "Empty system")
            | (lhs, rhs)::tail ->
                let cur = lhs, rhs in
                if is_equal_algebraic_term lhs rhs then impl tail resolved else 
                match (lhs, rhs) with 
                    Var a, any -> if memv a any then raise (NoSolution "Fourth rule abused") 
                            else let resolved = StringSet.add a resolved in
                            impl (List.append (apply_substitution_sys [a, any] tail) [cur]) resolved 
                    | any, Var a -> impl (List.append tail [rhs, lhs]) resolved 
                    | Fun(f, l1), Fun(g, l2) -> if f <> g || List.length l1 <> List.length l2 then raise (NoSolution "Third rule abused")
                                    else impl (List.append tail (get_args_sys l1 l2)) resolved in

    let dewrap sys =
        let rec impl sys ans =
            match sys with 
                [] -> List.rev ans
                | ((Var a, rhs)::tail) -> 
                    impl tail ((a, rhs)::ans) 
                | _ -> failwith "it's impossible, sorry" in
        impl sys [] in

    try 
        let resolved_system = impl sys StringSet.empty in
        print_string "";
        (Some (dewrap resolved_system))

    with (NoSolution msg) -> 
        print_string "";
        None;;