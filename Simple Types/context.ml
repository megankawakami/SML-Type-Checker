use "syntax.ml";

(* Bindings *)
datatype binding = 
	NameBind |
	VarBind of ty;

type context = (int * binding);

fun getbinding (ctx : context list) i = 
	#2 (hd (List.filter (fn x => #1 x = i) ctx));	

fun addbinding ctx x bind = (x, bind)::ctx;

fun getTypeFromContext ctx i = 
	case (getbinding ctx i) of
		VarBind(tyT) => tyT |
		_ => Error "getTypeFromContext: Wrong kind of binding for variable\n";

fun error msg = print(msg^"\n");