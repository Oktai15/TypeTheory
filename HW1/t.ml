open Hw1;;

print_int (int_of_peano (S (S (Z))));;
print_string "\n";;
print_int (int_of_peano (peano_of_int 5));;
print_string "\n";;
print_int (int_of_peano (inc (S(S(Z)))));;
print_string "\n";;
print_int (int_of_peano (add (S(S(Z))) (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (mul (S(S(S(S(Z))))) (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (power (S(S(S(Z)))) (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (power Z (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (power (S Z) (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (power (S Z) Z));;
print_string "\n";;
print_int (int_of_peano (power (S(S(S(Z)))) (S Z)));;
print_string "\n";;
print_int (int_of_peano (sub (S(S(S(S(S(S(S(Z)))))))) (S(S(S(Z))))));;
print_string "\n";;
print_list (rev [1;2;3;4;5;6;7]);;
(*
print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x"));;
*)
