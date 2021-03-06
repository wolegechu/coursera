(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, strList) =
  let fun sub(strList) =
	case strList of
	    [] => []
	  | x::xs' => if same_string(str,x)
		      then xs'
		      else x::sub(xs')
  in
      let val value = sub(strList)
      in
	  if value = strList
	  then NONE
	  else SOME(value)	   
      end
  end
			      

fun get_substitutions1 (strLists, str) =
  case strLists of
      [] => []
    | x::xs' => let val remain = all_except_option (str, x)
		    val result = get_substitutions1(xs', str)
		in
		    case remain of
			NONE => result
		      | SOME [] => result
		      | SOME a =>  a @ result
		end

fun get_substitutions2 (strLists, str) =
  let fun sub(strLists, acc) =
	case strLists of
	    [] => acc
	  | x::xs' =>  let val remain = all_except_option(str, x)
		       in
			   case remain of
			       NONE => sub(xs', acc)
			     | SOME [] => sub(xs', acc)
			     | SOME a => sub(xs', a @ acc)
		       end
  in
      sub(strLists,[])
  end

fun similar_names (strList, fullName:{first:string,middle:string,last:string}) =
  let val {first=a,middle=b,last=c} = fullName
  in
      let fun sub(strList, acc) =
	    case strList of
		[] => acc
	      | x::xs' => let val remain = all_except_option(a, x)
			  in
			      case remain of
				  NONE => sub(xs', acc)
				| SOME [] => sub(xs', acc)
				| SOME list => let fun sub_sub(list) =
						     case list of
							 [] => []
						       | y::ys' => {first=y,last=c,middle=b} :: sub_sub(list)
					       in
						   sub(xs', acc @ sub_sub(list))
					       end
			  end
      in
	  sub(strList,[{first=a,last=c,middle=b}])
      end
  end
      
  
  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
