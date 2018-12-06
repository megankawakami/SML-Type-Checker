(* Binary Tree datatype *)
datatype 'label btree = 
	Empty | 
	Node of 'label * 'label btree * 'label btree;

(* Compares two strings lowercase representation *)
fun lower(nil) = nil | 
	lower(c::cs) = (Char.toLower c)::lower(cs);

fun lookup lt Empty x = false | 
	lookup lt (Node(y, left, right)) x = 
		if lt(x, y) then lookup lt left x
		else if lt(y, x) then lookup lt right x
		else true;

fun strLT(x, y) = 
	implode(lower(explode x)) <
		implode(lower(explode y));

(* Checks if a String is a space or a Number *)
fun isSpace c = (c = #" ");

fun isNum str = List.all (Char.isDigit) (explode str);


(* Builds expression tree from parsed expression *)
(* INPUT: list (expression) *)
(* OUTPUT: Tree, rest of list *)
fun addTree(nil) = (Empty, nil) |
	addTree(x::nil) = 
		(Node(x, Empty, Empty), nil) |
	addTree(x::xs) = 
		if isNum(x) then (Node(x, Empty, Empty), xs)
		else let val (right, rest) = addTree(xs)
				 val (left, rest) = addTree(rest) in
			(Node(x, left, right), rest) end;

(* Parses Expression => returns expression tree *)
fun buildTree(expr) = 
	let val pExpr = rev (String.tokens isSpace expr)
		val (T, rest) =  addTree(pExpr) 
	in T end;

(* Evaluates expression from expression tree *)
	(* - if a node is a leaf, it must be an int *)
fun evalTree(Empty) = 0 |
	evalTree(Node(x, Empty, Empty)) = valOf (Int.fromString(x)) : int | 
	evalTree(Node(x, left, right)) = 
		let val left = evalTree(left)
			val right = evalTree(right) in 
		case x of
			"+" => left + right |
			"-" => left - right |
			"*" => left * right end;

(* Evaluates post-fix expression *)
(* string -> int *)
fun eval(expr) = 
	let val T = buildTree(expr) in 
	evalTree(T) end;

print("\n\n TESTS: \n");
print("Expected: 1 + (2 * 3) = " ^ Int.toString(1 + (2 * 3)) ^ "\n");
print("Actual: 1 2 3 * + = " ^ Int.toString(eval("1 2 3 * +")) ^"\n\n");

print("Expected: (1 + 2) - (3 + (4 * 5) = " ^ Int.toString((1 + 2) - (3 + (4 * 5))) ^ "\n");
print("Actual: 1 2 + 3 4 5 * + - = " ^ Int.toString(eval("1 2 + 3 4 5 * + -")) ^"\n\n");

print("Expected: (1 + 2) + (2 * 3) = " ^ Int.toString((1 + 2) + (2 * 3)) ^ "\n");
print("Actual: 1 2 + 2 3 * + = " ^ Int.toString(eval("1 2 + 2 3 * +")) ^ "\n\n");

(* Open interactive evaluation  
>> sml
- use "eval.ml";  *)
open TextIO; 

while true do 
	let val expr = inputLine stdIn in 
	case expr of NONE => print "0\n" |
				 SOME e =>  print("= "^ Int.toString(eval(String.substring(e, 0, (size e) - 1))) ^ "\n")
	end;





