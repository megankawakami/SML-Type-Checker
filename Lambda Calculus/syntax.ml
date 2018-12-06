(* Types of terms in Untyped lambda-calculus *)
datatype term =
	(* first int is a variables de Bruijn index *)
	(* Second int contains total length of the context in which the variable occurs *)
	TmVar of int * int
	(* string is hint for the name of the bound variable *)
  | TmAbs of string * term
  | TmApp of term * term;

