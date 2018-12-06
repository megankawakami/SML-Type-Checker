(* Describes where (what character postiion in which source file) 
	the node originate *)
type info = int;

(* Constructors name different sorts of nodes in AST of type term *)
datatype term = 
	TmTrue 
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term;