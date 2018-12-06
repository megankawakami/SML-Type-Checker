(* Types *)
datatype ty = 
	TyBool |
	TyArr of ty * ty |
	Error of string;

(* Terms *)
datatype term = 
	TmTrue |
	TmFalse |
	TmIf of term * term * term |
	TmVar of int * int |
	TmAbs of int * ty * term |
	TmApp of term * term;
