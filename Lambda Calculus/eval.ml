(* Import syntax structure *)
use "syntax.ml";

(* Shifting *)
fun termShift(d, t) = 
	let 
		fun walk(c, TmVar(x, n)) = 
				if x >= c then TmVar(x+d, n+d)
					      else TmVar(x, n+d) |
			walk(c, TmAbs(x, t1)) = 
				TmAbs(x, walk(c+1, t1)) |
			walk(c, TmApp(t1, t2)) = 
				TmApp(walk(c, t1), walk(c, t2))
	in 
		walk(0, t)
	end;

(* Substitution *)
fun termSubst(j, s, t) =
	let 
		fun walk(c, TmVar(x, n)) = 
				if x = j + c then termShift(c, s)
					      else TmVar(x, n) |
			walk(c, TmAbs(x, t1)) = 
				TmAbs(x, walk(c+1, t1)) |
			walk(c, TmApp(t1, t2)) = 
				TmApp(walk(c, t1), walk(c, t2))
	in 
		walk(0, t)
	end;


(* Evaluation *)
fun isval(ctx, TmAbs(_, _, _)) = true |
	isval(ctx, _) = false;


