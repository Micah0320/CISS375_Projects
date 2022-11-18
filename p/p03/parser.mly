/* file: calc1.mly */
/* Calculator */
/*Place HashTable Here*/
%{
    open Printf;;
    let sym_table = Hashtbl.create 100;;
    let getNum s = float_of_string (String.sub s 0 ((String.length s) - 1));;
    let getString s = (String.sub s 0 ((String.length s) - 1));;
%}
/* Ocamlyacc Declarations */
%token NEWLINE
%token PROGRAM
%token BEGIN
%token END
%token INTEGER
%token REAL
%token STRING
%token AOP
%token DELIM
%token <string> Id
%token WRITE
%token LPAREN RPAREN
%token <int> INT
%token <string> NUM
%token <string> STRING_VAL
%token EOF
%token PLUS MINUS MULTIPLY DIVIDE CARET
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG /* negation -- unary minus */
%right CARET /* exponentiation */
%start input
%type <unit> input
/* Grammar follows */
%%
input:
PROGRAM decider BEGIN getLine END DELIM eof{ printf "--- No syntax errors ---\n"; flush stdout; raise End_of_file}
/*|NEWLINE decider {}*/
|/* empty */ { }
  | error NEWLINE { let start_pos = Parsing.rhs_start_pos 3 in
printf "**Error** %d: Missing Keyword \n"
start_pos.pos_lnum; flush stdout; raise End_of_file}
;
getLine: 
line getLine{}
  |NEWLINE getLine{}
|next {}
;

line:
WRITE exp DELIM{ if String.ends_with "I" $2
                 then
                   (printf "\t%s\n" (string_of_int (int_of_float(getNum $2))); flush stdout;)
                 else if String.ends_with "R" $2 then
                   (printf "\t%s\n" (string_of_float ((getNum $2))); flush stdout;)}
  |exp DELIM{}
  |WRITE STRING_VAL DELIM {printf "\t%s\n" $2; flush stdout;}
  |Id AOP exp DELIM {if (Hashtbl.mem sym_table $1)
                     then
                       if ((String.ends_with "I" (Hashtbl.find sym_table $1) && String.ends_with "I" $3) || (String.ends_with "R" (Hashtbl.find sym_table $1) && String.ends_with "R" $3))
                          then (Hashtbl.replace sym_table $1 ($3);)
                           else let start_pos = Parsing.rhs_start_pos 3 in
                          printf "**Error** %d: The assignment of different data types\n"
                                 start_pos.pos_lnum; flush stdout; raise End_of_file;
                     else let start_pos = Parsing.rhs_start_pos 3 in
                          printf "**Error** %d: Use of undeclared identifier %s\n"
                                 start_pos.pos_lnum $1; flush stdout; raise End_of_file }

|Id AOP STRING_VAL DELIM {if (Hashtbl.mem sym_table $1) then (Hashtbl.replace sym_table $1 ($3 ^ "S");) else let start_pos = Parsing.rhs_start_pos 3 in
printf "**Error** %d: Use of undeclared identifier %s\n"
start_pos.pos_lnum $1; flush stdout; raise End_of_file }


  |WRITE Id DELIM{ if  not (Hashtbl.mem sym_table $2)
                   then (let start_pos = Parsing.rhs_start_pos 3 in
                        printf "**Error** %d: Use of undeclared identifier %s\n"
                               start_pos.pos_lnum $2; flush stdout; raise End_of_file;)
                     else
                     if String.ends_with "R" (Hashtbl.find sym_table $2)
                     then (printf "\t %s \n" (String.sub (Hashtbl.find sym_table $2) 0 ((String.length (Hashtbl.find sym_table $2)) - 1) ); flush stdout)
                     else
                       if String.ends_with "I" (Hashtbl.find sym_table $2)
                       then (printf "\t %s \n" (string_of_int (int_of_float (getNum (Hashtbl.find sym_table $2) ))) ; flush stdout)
                       else if String.ends_with "S" (Hashtbl.find sym_table $2)
                       then (printf "\t %s \n" (getString (Hashtbl.find sym_table $2)) ; flush stdout)
                     }

  |error { let start_pos = Parsing.rhs_start_pos 3 in
printf "**Error** %d: Something went Wrong\n"
start_pos.pos_lnum;}
;

next:/* empty */ {}
decider: declarations decider{}
  | NEWLINE decider{ }
|next {}
;
declarations:
INTEGER Id DELIM{if (Hashtbl.mem sym_table $2) then (let start_pos = Parsing.rhs_start_pos 3 in
printf "**Error** %d: Redefinition Error of Variable %s \n"
start_pos.pos_lnum $2; flush stdout; raise End_of_file) else (Hashtbl.add sym_table $2 "0I";)}
  |REAL Id DELIM{if (Hashtbl.mem sym_table $2) then (let start_pos = Parsing.rhs_start_pos 3 in
printf "**Error** %d: Redefinition Error of Variable %s\n"
start_pos.pos_lnum $2; flush stdout; raise End_of_file) else (Hashtbl.add sym_table $2 "0R";)}
  |STRING Id DELIM{if (Hashtbl.mem sym_table $2) then (let start_pos = Parsing.rhs_start_pos 3 in
printf "**Error** %d: Redefinition Error of Variable %s \n"
start_pos.pos_lnum $2; flush stdout; raise End_of_file) else (Hashtbl.add sym_table $2 "0S";)}
/*|BEGIN{ printf "\t Begin\n"; flush stdout}*/
  
;

exp: NUM { $1 }
| Id {if (Hashtbl.mem sym_table $1)
        then Hashtbl.find sym_table $1
        else let start_pos = Parsing.rhs_start_pos 3 in
             printf "**Error** %d:Use of undeclared identifier %s\n"
                    start_pos.pos_lnum $1; flush stdout; raise End_of_file }
  | exp PLUS exp {if ((String.ends_with "R" $1) &&  (String.ends_with "R" $3))
                  then  (string_of_float((getNum $1) +. (getNum $3)) ^ "R")
                  else
                    if (String.ends_with "I" $1 && String.ends_with "I" $3)
                    then (string_of_float ((getNum $1) +. (getNum $3)) ^ "I")
                 else let start_pos = Parsing.rhs_start_pos 3 in
                      printf "**Error** %d: Addition of different data types\n"
                             start_pos.pos_lnum; flush stdout; raise End_of_file }
| exp MINUS exp {if ((String.ends_with "R" $1) &&  (String.ends_with "R" $3))
                  then  (string_of_float((getNum $1) -. (getNum $3)) ^ "R")
                  else
                    if (String.ends_with "I" $1 && String.ends_with "I" $3)
                    then (string_of_float ((getNum $1) -. (getNum $3)) ^ "I")
                 else let start_pos = Parsing.rhs_start_pos 3 in
                      printf "**Error** %d: Subtraction of different data types\n"
                             start_pos.pos_lnum; flush stdout; raise End_of_file }
| exp MULTIPLY exp {if ((String.ends_with "R" $1) &&  (String.ends_with "R" $3))
                  then  (string_of_float((getNum $1) *. (getNum $3)) ^ "R")
                  else
                    if (String.ends_with "I" $1 && String.ends_with "I" $3)
                    then (string_of_float ((getNum $1) *. (getNum $3)) ^ "I")
                 else let start_pos = Parsing.rhs_start_pos 3 in
                      printf "**Error** %d: Multiplication of different data types\n"
                             start_pos.pos_lnum; flush stdout; raise End_of_file }
| exp CARET exp {if ((String.ends_with "R" $1) && (String.ends_with "R" $3))
                  then (string_of_float((getNum $1) ** (getNum $3)) ^ "R")
                  else
                    if (String.ends_with "I" $1 && String.ends_with "I" $3)
                    then (string_of_float ((getNum $1) ** (getNum $3)) ^ "I")
                 else let start_pos = Parsing.rhs_start_pos 3 in
                      printf "**Error** %d: Incorrect Typing\n"
                             start_pos.pos_lnum; flush stdout; raise End_of_file }
| exp DIVIDE exp {if ((String.ends_with "R" $1) &&  (String.ends_with "R" $3))
                 then if getNum $3 <> 0.0
                      then (string_of_float((getNum $1) /. (getNum $3)) ^ "R")
                      else let start_pos = Parsing.rhs_start_pos 3 in
                      printf "**Error** %d: Cannot divide by zero\n"
                             start_pos.pos_lnum; flush stdout; raise End_of_file
                  else
                    if (String.ends_with "I" $1 && String.ends_with "I" $3)
                    then if getNum $3 <> 0.0
                         then(string_of_float ((getNum $1) /. (getNum $3)) ^ "I")
                         else let start_pos = Parsing.rhs_start_pos 3 in
                              printf "**Error** %d: Cannot divide by zero\n"
                                     start_pos.pos_lnum; flush stdout; raise End_of_file
                 else let start_pos = Parsing.rhs_start_pos 3 in
                      printf "**Error** %d: Division of different data types\n"
                             start_pos.pos_lnum; flush stdout; raise End_of_file }
  | MINUS exp %prec NEG {if (String.ends_with "R" $2)
                         then (string_of_float (-. getNum ($2)) ^ "R")
                         else  (string_of_float (-. getNum ($2)) ^ "I")}
  | LPAREN exp RPAREN { $2 }
;
eof:
EOF { raise End_of_file}
|error {}
%%
