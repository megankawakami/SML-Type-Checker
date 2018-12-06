(* Import syntax structure *)
use "syntax.ml";

(* Checks whether a term is a numeric value *)
fun isnumericval TmZero = true |
	isnumericval (TmSucc(t1)) = isnumericval t1 |
	isnumericval _ = false;

(* Checks whether a term is a value *)
fun isval TmTrue = true |
	isval TmFalse = true |
	isval t = if (isnumericval t) then true
								  else false;

exception NoRuleApplies;

val dummyinfo = 0 : info;

(* Single-step evaluator *)
fun eval1 (TmIf(TmTrue, t2, t3)) = t2 | 
 	eval1 (TmIf(TmFalse, t2, t3)) = t3 |
 	eval1 (TmIf(t1, t2, t3)) = 
 		let val t1' = eval1 t1 in 
 			TmIf(t1', t2, t3) end |
 	eval1 (TmSucc(t1)) = 
 		let val t1' = eval1 t1 in
 			TmSucc(t1') end |
 	eval1 (TmPred(TmZero)) = 
 		TmZero |
 	eval1 (TmPred(TmSucc(nv1))) = 
 		if (isnumericval nv1) then nv1 else
 			let val t1' = eval1 (TmSucc(nv1)) in
 				TmPred(t1') end |
 	eval1 (TmPred(t1)) = 
 		let val t1' = eval1 t1 in 
 			TmPred(t1') end |
 	eval1 (TmIsZero(TmZero)) = 
 		TmTrue |
 	eval1 (TmIsZero(TmSucc(nv1))) =
 		if (isnumericval nv1) then TmFalse else
 			let val t1' = eval1 (TmSucc(nv1)) in 
 				TmIsZero(t1') end |
 	eval1 (TmIsZero(t1)) = 
 		let val t1' = eval1 t1 in 
 			TmIsZero(t1') end |
 	eval1 _ = raise NoRuleApplies;

fun eval t = 
	let val t' = eval1 t  in
		(eval t') end
	handle NoRuleApplies => t;

print("\nCall expressions with 'eval'.\n \n");












