use "syntax.ml";
use "context.ml";

fun typeof ctx t = 
	case t of
		TmTrue => TyBool |
		TmFalse => TyBool |
		TmIf(t1, t2, t3) =>
			if (typeof ctx t1) = TyBool then
				let 
					val tyT2 = typeof ctx t2 
				in 
					if tyT2 = (typeof ctx t3) then tyT2
					else Error "arms of conditional have different types"
				end
			else Error "guard of conditional not a boolean" |
		TmVar(i, _) => getTypeFromContext ctx i |
		TmAbs(x, tyT1, t2) => 
			let 
				val ctx' = addbinding ctx x (VarBind(tyT1))
			in 
				let 
					val tyT2 = typeof ctx' t2
				in 
					TyArr(tyT1, tyT2)
				end
			end |
		TmApp(t1, t2) => 
			let 
				val tyT1 = typeof ctx t1 
			in 
				let
					val tyT2 = typeof ctx t2 
				in 
					case tyT1 of
						TyArr(tyT11, tyT12) => 
							if tyT2 = tyT11 then tyT12
											else Error "parameter type mismatch" |
						_ => Error "arrow type expected"
				end
			end;









(* fun typeof ctx TmTrue = TyBool |
	typeof ctx TmFalse = TyBool |
	typeof ctx (TmIf(t1, t2, t3)) =
		if (typeof ctx t1) = TyBool then
			let 
				val tyT2 = typeof ctx t2 
			in 
				if tyT2 = (typeof ctx t3) then tyT2
				else Error "arms of conditional have different types"
			end
		else Error "guard of conditional not a boolean" |
	typeof ctx (TmVar(i, _)) = getTypeFromContext ctx i |
	typeof ctx (TmAbs(x, tyT1, t2)) = 
		let 
			val ctx' = addbinding ctx x (VarBind(tyT1))
		in 
			let 
				val tyT2 = typeof ctx' t2
			in 
				TyArr(tyT1, tyT2)
			end
		end |
	typeof ctx (TmApp(t1, t2)) = 
		let 
			val tyT1 = typeof ctx t1 
		in 
			let
				val tyT2 = typeof ctx t2 
			in 
				case tyT1 of
					TyArr(tyT11, tyT12) => 
						if tyT2 = tyT11 then tyT12
										else Error "parameter type mismatch" |
					_ => Error "arrow type expected"
			end
		end;







 *)