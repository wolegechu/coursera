fun is_older (x: int*int*int, y: int*int*int) =
  if #1 x = #1 y
  then
      if #2 x = #2 y then #3 x < #3 y
      else #2 x < #2 y
  else #1 x < #1 y
			    	  
fun number_in_month ([], mo) = 0
  | number_in_month ((x,y,z)::rest, mo) =
    if y = mo then 1 + number_in_month(rest, mo)
    else 0 + number_in_month(rest, mo)


fun number_in_months (dates, []) = 0
  | number_in_months (dates, mo::rest) =
    number_in_month(dates, mo) + number_in_months(dates, rest)

fun dates_in_month ([], mo) = []
  | dates_in_month ((x,y,z)::rest, mo) =
    if y = mo then (x,y,z)::dates_in_month(rest, mo)
    else dates_in_month(rest, mo)

fun dates_in_months (dates, []) = []
  | dates_in_months (dates, mo::rest) =
    let fun append (xs, ys) =
	  if null xs
	  then ys
	  else (hd xs)::append(tl xs, ys)
    in
	append(dates_in_month(dates, mo), dates_in_months(dates, rest))
    end


fun get_nth (str, 1) = hd str
  | get_nth (str, n) =
    get_nth (tl str, n-1)
	    
fun date_to_string (y, m, d) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, m)^" "^Int.toString(d)^", "^Int.toString(y)
  end

fun number_before_reaching_sum (sum, []) = 0
  | number_before_reaching_sum (sum, x::rest) =
    if sum <= 0 then ~1
    else 1 + number_before_reaching_sum(sum - x, rest)
			

fun what_month (d) =
  let val m_len = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      1 + number_before_reaching_sum(d, m_len)
  end

fun month_range (day1, day2) =
  if day1 = day2 then [what_month (day1)]
  else what_month (day1) :: month_range (day1+1, day2)

fun oldest ([]) = NONE
  | oldest (x::rest) =
    if rest = [] then SOME x
    else let val old_one = oldest(rest)
	 in
	     if is_older(x, valOf old_one) then SOME x
	     else old_one
	 end
