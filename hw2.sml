(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
(* a *)
fun string_in_list(str,[]) = false
	| string_in_list(str,x::xs) =
			x=str orelse string_in_list(str,xs)

fun remove_string_from_list (str,[]) = []
	| remove_string_from_list (str,x::xs) = (* if x is not str, add it to the result*)
		if same_string(x,str) then remove_string_from_list(str,xs)
		else x::remove_string_from_list(str,xs)

fun all_except_option (str,strs) =
	if string_in_list(str,strs)
	then SOME (remove_string_from_list(str,strs))
	else NONE

(* b *)
fun get_substitutions1 ([],str) = []
	|	get_substitutions1 (x::xs,str) = (* if list has at least one element *)
		case all_except_option(str,x) of (* get the data of function 1a through a case statement *)
			SOME(y) => y@get_substitutions1(xs,str) (* y is a list *)
		|	NONE => get_substitutions1(xs,str)

(* c *)
fun get_substitutions2 ([],str) = []
	| get_substitutions2 (x::xs,str) =
		let
			fun aux (thelist,str,acc) =
				case thelist of
					[] => acc
				| x::xs =>
					case all_except_option (str,x) of (* get the data of function 1a through a case statement*)
						SOME(y) => aux(xs,str,acc@y) (*at this step acc is known, y is known and no recursive calls are involved in calculations *)
					|	NONE => aux(xs,str,acc)
		in
			aux (x::xs,str,[])
		end

(* d *)
fun similar_names (subs,fullname:{first:string,middle:string,last:string}) =
	let
		val {first=x,middle=y,last=z} = fullname
		val substi = get_substitutions1(subs,x)	(* get the list of subs *)
		val firstnamelist = x::substi (* This is the full list of first names (initial and subs of it )*)
		fun aux ([],{first=x,middle=y,last=z}) = []
		| 	aux (t::ts,{first=x,middle=y,last=z}) =
				{first=t,middle=y,last=z}::aux(ts,{first=x,middle=y,last=z})
	in
		aux(firstnamelist,fullname)
	end

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(* a *)
fun card_color (x,_) =
	case x of
			Clubs => Black
		|	Spades => Black
		|	Diamonds => Red
		|	Hearts => Red

(* b *)
fun card_value (_,y) =
	case y of
			Num(value) => value
		| Ace => 11
		| _ => 10

(* c *)
(*
fun remove_card ([],c,e) = []
| remove_card ((ca::cs),c,e) =
  let
    fun card_found([],x) = false
      | card_found(t::ts,x) =
      x=t orelse card_found(ts,x)
  in
    if not(card_found ((ca::cs),c)) then raise e
    else
      if ca=c then cs else ca::(remove_card (cs,c,e))
      (* if ca=c then remove_card (cs,(Clubs,~1),e) else ca::rest *)
      (* if the card has been removed, remove a card that doesn't exist so as to keep the rest of the list intact *)
  end
*)

fun remove_card (cs,c,e) =
  let
  (* remove only the first occurence of c in cs *)
    fun aux []=[]
    | aux (l::ls)=
      if l=c then ls
      else l::(aux ls)
  in
    let
    (* tempList is a list that does not contain card c,
     whether that existed in first list cs or not *)
      val tempList = aux cs
    in
    (* if cs and tempList have the same Length, c never existed in cs *)
      if length tempList = length cs then raise e
      else tempList
    end
  end

(* d *)
fun all_same_color cs =
 case cs of
 [] => true
| _::[] => true
| c1::(c2::cc) => ((card_color c1 = card_color c2) andalso all_same_color (c2::cc))

(* e *)
fun sum_cards cs =
  let
    fun tail_add (acc,[]) = acc
    | tail_add (acc,(c::cs)) = tail_add ((acc+(card_value c)),cs)
  in
    tail_add (0,cs)
  end

(* f *)
fun score (cs,goal) =
  let
    val prelim =
      let
        val sum = sum_cards cs
      in
        if sum > goal then 3*(sum-goal) else goal-sum
      end
  in
    if all_same_color cs then prelim div 2 else prelim
  end

(* g*)
fun officiate (cs,ms,goal) = (* cs:card-list, ms:move-list *)
  let
    fun help_off (cs,hs,[]) = score (hs,goal) (* cs:card-list, hs:held-cards *)
    | help_off ([],hs,(Discard c::ms)) = help_off (cs,remove_card (hs,c,IllegalMove),ms)
    | help_off ([],hs,(Draw::ms)) = score (hs,goal) (* Did pattern-matching in the triple argument because
      SML confused the BAR (|) of case expression with the function definition*)
    | help_off ((c::cs),hs,(m::ms)) =
        case m of
          Discard c => help_off (cs,remove_card (hs,c,IllegalMove),ms)
        | Draw =>
          if sum_cards (c::hs) > goal then score (c::hs,goal)
          else help_off (cs,c::hs,ms)
  in
    help_off (cs,[],ms)
  end
