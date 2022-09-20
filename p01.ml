(*
   Micah Arndt
   p01.ml
   *)

let get_input = read_line;;

(*This Gets the File that serves as input*)
let file_name = get_input();;
let file = String.concat file_name [""; ".mycpp"];;
let write_file_name = String.concat file_name [""; ".bac"];;

(*
   Reads a file line by line and then puts it in a list
   *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines
;;

(*Testing for printing the file*)
  let rec print_str_list a = match a with
      [] -> print_newline
    |a::body -> let _ = print_string (String.concat a [""; "\n"]) in print_str_list body 
;;

(*Return a list of Strings*)
let x = read_file file;;

let testWrite file_write message =
  (* Write message to file *)
  let oc = open_out file_write in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s\n" message;
  (* write something *)
  close_out oc;
;;


(*This will get all the strings in a list and then combine them
  into one large string. This will also be responsible for creating
  the write rules*)

(*
   Rules to Consider:
   // -> Single Line Comment- Replace with one space and skip line
   /**/ -> Multi-line Coment -> Set Comment flag until you see a */ Replace with
                                a space
   /n, ,/t -> replace with one space

   only one whitespace tab should be written in between non-whitespace chars.
   Bool Flag will keep track of this
   
*)

let processString string =
  let rec pString string i singleCommentActive multiActive stringMode acc = 
    if (i < String.length string) then
      match string.[i] with
      (*Space Case*)
        ' ' -> if String.length acc > 0 then
          if acc.[(String.length acc) - 1] != ' '
          then (pString string (i + 1) singleCommentActive multiActive stringMode(String.concat acc [""; " "]))
          else (pString string (i + 1) singleCommentActive multiActive stringMode acc)
        else(pString string (i + 1) singleCommentActive multiActive stringMode (String.concat acc [""; " "]))
      (* Tab Case*)
      | '\t' -> if String.length acc > 0 then if acc.[(String.length acc) - 1] != ' '
          then (pString string (i + 1) singleCommentActive multiActive stringMode (String.concat acc [""; " "]))
          else (pString string (i + 1) false multiActive stringMode acc)
        else (pString string (i + 1) singleCommentActive multiActive stringMode (String.concat acc [""; " "]))
      (* Newline Case *)
      | '\n' -> if String.length acc > 0 then if (acc.[(String.length acc) - 1] != ' ' && stringMode == false)
          then (pString string (i + 1) false multiActive stringMode (String.concat acc [""; " "]))
          else if(stringMode == true) then (pString string (i + 1) false multiActive stringMode (String.concat acc [""; " "]))
          else (pString string (i + 1) false multiActive stringMode acc)
        else (pString string (i + 1) false multiActive stringMode (String.concat acc [""; " "]))
      (* Comment Case*)
      | '/' -> if String.length acc > 0 then if (string.[i + 1] == '/' && acc.[(String.length acc) - 1] != ' ') then pString string (i + 2) true multiActive stringMode (String.concat acc [""; " "])
          else if (string.[i + 1] == '/' && acc.[(String.length acc) - 1] == ' ' && stringMode == false)
          then pString string (i + 2) true multiActive stringMode acc
          else if (string.[i+1] == '*' && stringMode == false) then pString string (i + 2) false true stringMode acc
          else if stringMode then pString string (i + 1) false multiActive stringMode (String.concat acc [""; "/"])
          else String.concat acc  [""; "ERROR: Invalid Comment Syntax"]
        else if (string.[i + 1] == '/' && stringMode == false) then (pString string (i + 2) true multiActive stringMode (String.concat acc [""; " "])) else String.concat acc [""; "ERROR: Invalid Comment Syntax"]
      |'*' -> if (string.[i + 1] == '/' && stringMode == false) then (pString string (i + 2) false false stringMode acc ^ " ") else if (string.[i + 1] == '/' && stringMode) then pString string (i + 2) false false true (String.concat acc ["";"//"])
        else if stringMode then ((pString string (i + 1) false false stringMode (String.concat acc [""; "*"])))
        else acc ^ "ERROR: Invalid Comment Syntax"
      (*String Case *)
      | '"'-> if (singleCommentActive == false && multiActive == false) then
          if stringMode then pString string (i + 1) false false false (String.concat acc [""; "\""])
          else pString string (i + 1) false false true (String.concat acc[""; "\""])
        else pString string (i + 1) singleCommentActive multiActive false acc
      | _ -> if (singleCommentActive == false && multiActive == false) then (pString string (i + 1) singleCommentActive multiActive stringMode (String.concat acc [""; (String.make 1 string.[i])])) 
            else (pString string (i + 1) true multiActive stringMode acc)
    else if (multiActive == true) then acc ^ "ERROR: Unending Comment"
    else
      acc
  in pString string 0 false false false ""
;;

  
(*Gets the code as one big string*)
let rec messageConcat message_list acc = match message_list with
    [] -> acc
  |x::xs ->  messageConcat xs (String.concat acc [""; x]);;

let m = (messageConcat x "");;
testWrite write_file_name (processString m);;



