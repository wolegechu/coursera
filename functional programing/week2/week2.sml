fun is_older (dateA : (int * int * int), dateB : (int * int * int)) = 
	if ((#1 dateA) < (#1 dateB)) then true
	else if ((#1 dateA) > (#1 dateB)) then false
	else
	    if ((#2 dateA) < (#2 dateB)) then true
	    else if ((#2 dateA) > (#2 dateB)) then false
	    else
		if ((#3 dateA) < (#3 dateB)) then true
		else if ((#3 dateA) > (#3 dateB)) then false
		else false

fun number_in_month (dates :(int * int * int) list,month : int) =
  if null dates then 0
  else
      let val rest = number_in_month(tl dates,month)
      in
	  let val head = hd dates
	  in
	      if (#2 head) = month
	      then 1 + rest
	      else rest
	  end
      end
	  
fun check_repeat (data : int list,check_item : int) =
  if null data
  then false
  else
      let val head = hd data
      in
	  if check_item = head
	  then true
	  else check_repeat(tl data,check_item)
      end

fun number_in_months (dates : (int * int * int) list,months : int list) =
  if null dates
  then 0
  else
      let val rest = number_in_months(tl dates,months)
      in
	  let val head = hd dates
	  in
	      let fun month_in_numbers(month : int,numbers : int list) =
		  if null numbers then 0
		  else
		      let val head = hd numbers
		      in
			  if month = head then 1
			  else month_in_numbers(month,tl numbers)
		      end
	      in
		 rest + month_in_numbers(#2 head,months)
	      end
	  end
      end

fun dates_in_month (dates : (int * int * int) list,month : int) =
  if null dates then []
  else
      let val head = hd dates
      in
	  let val rest = dates_in_month(tl dates,month)
	  in
	      if (#2 head) = month
	      then head :: rest
	      else rest
	  end
      end

fun dates_in_months (dates : (int * int * int) list,months : int list) =
  if null months  then []
  else
      let val head = hd months
      in
	  let val rest = dates_in_months(dates,tl months)
	  in
	      if check_repeat(tl months,head) = false
	      then let val date = dates_in_month(dates,head)
		   in
		       hd date :: rest
		   end
	      else rest
	  end
      end
	  
fun dates_in_months_challenge (dates : (int * int * int) list,months : int list) =
  if null months  then []
  else
      let val head = hd months
      in
	  let val rest = dates_in_months(dates,tl months)
	  in
	      if check_repeat(tl months,head) = false
	      then let val date = dates_in_month(dates,head)
		   in
		       hd date :: rest
		   end
	      else rest
	  end
      end
fun get_nth (text : string list,nth : int) =
  let val head = hd text
  in
      if nth = 1
      then head
      else get_nth(tl text,nth-1)
  end
      
fun date_to_string (year : int, month : int, day : int) =
  let val list_months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
      get_nth(list_months,month) ^ " " ^Int.toString(day) ^ ", " ^ Int.toString(year)
  end

fun number_before_reaching_sum (sum : int,numbers : int list) =
  let fun f_sum(sub_numbers : int list,original : int, degree : int) =
	if original <= 0 then degree - 1
	else
	    f_sum(tl sub_numbers,original -  (hd sub_numbers),degree + 1)
  in
      f_sum(numbers,sum,0)
  end
      
fun what_month (day : int) =
  number_before_reaching_sum(day,[31,28,31,30,31,30,31,30,31,30,31,31]) + 1
									      
fun month_range (day1 : int, day2 : int) =
  if day1 > day2 then []
  else
      what_month day1 :: month_range(day1+1,day2)

fun oldest (data : (int * int * int) list) =
  if null data
  then NONE
  else
      let fun sub_oldest(sub_data : (int*int*int) list,date : (int * int * int)) =
	    if null sub_data
	    then date
	    else if is_older(hd sub_data,date)
	    then sub_oldest(tl sub_data,hd sub_data)
	    else sub_oldest(tl sub_data,date)
      in
	 SOME(sub_oldest(tl data,hd data))
      end

fun reasonable_date(date : (int * int * int)) = true
