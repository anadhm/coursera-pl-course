(*1*)
fun is_older (a:int*int*int,b:int*int*int) =
	if #1 a < #1 b then true (* if year a is a smaller number than year b*)
	else if #1 a > #1 b then false (* if year b is a smaller number than year a*) 
	else (*year a = year b*)
		if #2 a <  #2 b then true (*same for months*)
		else if #2 a > #2 b then false
		else
			if #3 a < #3 b then true
			else false (* day a >= day b*)

(*2*)	
fun number_in_month (xs:(int*int*int) list, y:int) =
	if null xs then 0
	else
		if #2(hd xs)=y (*check if it's an appropriate date*)
		then 1+number_in_month((tl xs),y) (* if it is, add 1 to the counter *)
		else number_in_month((tl xs),y) (* else *add* 0 to the counter *)

(*3*)
fun number_in_months (xs:(int*int*int) list, ys:int list)=
	if null ys orelse null xs 
	then 0
	else
		number_in_month(xs,(hd ys))+number_in_months(xs,(tl ys))

(*4*)	
fun dates_in_month (xs:(int*int*int) list, y:int) =
	if null xs (* if starting list is empty obviously the resulting list will be empty as well *)
	then []
	else
	 	let 
	 		val ans=dates_in_month(tl xs,y) (* don't make the same recursive call twice *)
	 	in
	 		if (#2 (hd xs))=y 
	 		then (hd xs)::ans (* check if it's an appropriate date, then add it to the list*)
	 		else ans (* or leave the list as it is *)
	 	end

(*5*)
fun dates_in_months (xs:(int*int*int) list, ys:int list) =
	if null ys 
	then []
	else
		dates_in_month(xs, (hd ys))@dates_in_months(xs,(tl ys))

(*6*)
fun get_nth (xs:string list,n:int) =
 		if n=1 
 		then hd xs
 		else get_nth((tl xs),n-1)

(*7*)
fun date_to_string (x:int*int*int) =
	let 
		val	months=["January","February","March","April","May","June","July","August","September"
		,"October","November","December"]
	in
		get_nth(months,(#2 x))^" "^Int.toString(#3 x)^", "^Int.toString(#1 x)
	end	

(*8*)
fun number_before_reaching_sum (sum:int, xs:int list) =
	if sum-(hd xs)>0 (* if head of the list is not smaller than the *current* sum*)
	then number_before_reaching_sum((sum-(hd xs)),(tl xs))+1 (* then result is incremented by one and we check the rest of the list, until *current* sum reaches <=0 *)
	else 0 (* we have reached *current* sum <=0 so stop adding to the result *)

(*9*)
fun what_month (day:int) =
	let
		val cumulativedays=[31,28,31,30,31,30,31,31,30,31,30,31]
	in
		number_before_reaching_sum(day,cumulativedays) +1
	end
	
(*10*)
fun month_range (day1:int,day2:int) =
	if day1 > day2
	then []
	else
			(what_month (day1))::month_range(day1+1,day2)

(*11*)
fun oldest (xs:(int*int*int) list) =
	if null xs
	then NONE
	else
		let (* fine to assume argument nonempty because it is local *)
			fun min_date (xs : (int*int*int) list) =
				if null (tl xs) (* xs better not be [] *)
				then hd xs
				else 
					let val tl_ans = min_date(tl xs)
			    in
				 		if is_older((hd xs),tl_ans)
				 		then hd xs
				 		else tl_ans
			    end
		 in
	    SOME (min_date xs)
		 end
