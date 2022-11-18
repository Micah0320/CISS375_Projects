(*Input from a file, line by line*)
let get_input = read_line;;

(*This Gets the File that serves as input*)
let file_name = get_input();;
let file = String.concat file_name [""; ".mycpp"];;

let main file =
try
let lexbuf = Lexing.from_channel (open_in file) in
while true do
Parser.input Lexer.token lexbuf
done
with End_of_file -> exit 0
;;
let _ = Printexc.print main file
