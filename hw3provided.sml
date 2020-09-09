(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
fun only_capitals strs =
	List.filter (fn str => Char.isUpper (String.sub (str,0))) strs

(* 2 *)
fun longest_string1 [] = "" (* Here x and y are strings *)
| longest_string1 strs = List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" strs

(* 3 *)
fun longest_string2 [] = ""
| longest_string2 strs = List.foldl (fn (x,y) => if String.size x < String.size y then y else x) "" strs

(* 4 *)
fun longest_string_helper f [] = ""
| longest_string_helper f strs = List.foldl f "" strs
(* Singature should be (int * int -> bool) -> string list -> string,
couldn't figure out how to pass the length of the string to the anonymous function *)
val longest_string3 = longest_string_helper (fn (x,y) =>if String.size x > String.size y then x else y)

val longest_string4 = longest_string_helper (fn (x,y) =>if String.size  y > String.size x then y else x)

(* 5 *)
val longest_capitalized =	longest_string1 o only_capitals

(* 6 *)
val rev_string =	String.implode o rev o String.explode

(* 7 *)
fun first_answer f [] = raise NoAnswer
| first_answer f (l::ls) =
	case f l of
	SOME v => v
	| NONE => first_answer f ls

(* 8 *)
fun all_answers f [] = SOME []
| all_answers f ls =
	let
		fun aux acc [] = acc
		| aux acc (x::xs) =
		let
			val new_acc = (case f x of SOME y => y@acc
																| NONE => acc )
		in
			aux new_acc xs
		end
		val temp = aux [] ls
	in if length temp < length ls then NONE else SOME temp
	end

(* 9 *)
fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
	g (fn () => 1) (fn x => String.size x) p

fun count_some_var (str,p) = g (fn () => 0) (fn x => if x=str then 1 else 0) p

(* 10 *)
fun check_pat p =
	let
	 	fun take_strings pat =
			case pat of
				Variable str => [str]
				| TupleP (p::ps) => (take_strings p)@(take_strings (TupleP ps))
				| ConstructorP (_,p) => take_strings p
				| _ => []

		fun not_repeat [] = true
		| not_repeat (l::ls) =
			not(List.exists (fn x => x=l) ls) andalso (not_repeat ls)
	in
		not_repeat (take_strings p)
	end

(* 11 *)

fun match (Unit,UnitP) = SOME []
| match (_,Wildcard) = SOME []
| match (v,Variable s) = SOME [(s,v)]
| match (Const x1,ConstP x2) = if x1=x2 then SOME [] else NONE
| match (Tuple vs,TupleP ps) =
		if length vs = length ps then all_answers match (ListPair.zip (vs,ps))
		else NONE
| match (Constructor (s2,v),ConstructorP (s1,p)) =
	if s1=s2 then match (v,p) else NONE
| match (_,_) = NONE

(* 12 *)
fun first_match (v,ps) = (* create a list of [v,v,..] and zip it with the pattern list *)
	SOME (first_answer match (ListPair.zip ((List.tabulate ((length ps),(fn x => v))),ps)))
	handle NoAnswer => NONE
