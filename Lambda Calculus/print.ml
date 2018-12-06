(* Import syntax structure *)
use "syntax.ml";

let rec printtm ctx t = match t with 
	TmAbs(fi,x,t1) →
        let (ctx’,x’) = pickfreshname ctx x in
        pr "(lambda "; pr x’; pr ". "; printtm ctx’ t1; pr ")"
| TmApp(fi, t1, t2) →
              pr "("; printtm ctx t1; pr " "; printtm ctx t2; pr ")"
| TmVar(fi,x,n) →
              if ctxlength ctx = n then
                pr (index2name fi ctx x)
              else
                pr "[bad index]"