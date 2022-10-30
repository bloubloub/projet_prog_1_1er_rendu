(** Used in main to give tokens to the parser.
Implemented a character counter, under the reference nb_chars, enabling to keep track of the position of lexical mistakes.
Words a letter short to be the keywords float or int triggers a special error message.
Spaces, tablations and '\n' are considered as mistakes, since not consequently in the exp definition of the subject.
An exception was made for the case of '\n' as last character before end of file, for practical reasons. **)

{
 open Parser_toutatisser
 let nb_chars = ref 1
 exception Invalid_keyword of string
 exception Invalid_character of string
}

let digit = ['0'-'9']

rule token = parse
 | digit+ '.' digit* as x { nb_chars := !nb_chars + String.length x ; Flt (float_of_string x) }
 | digit+ as x { nb_chars := !nb_chars + String.length x ; Integ (int_of_string x) }
 | 'f' 'l' 'o' 'a' 't' { nb_chars := !nb_chars + 5 ; Convflt }
 | 'i' 'n' 't' { nb_chars := !nb_chars + 3 ; Convint }
 | '-' { incr nb_chars ; Minusint }
 | '-' '.' { incr nb_chars ; incr nb_chars ; Minusflt }
 | '+' { incr nb_chars ; Plusint }
 | '+' '.' { incr nb_chars ; incr nb_chars ; Plusflt }
 | '*' { incr nb_chars ; Timesint }
 | '*' '.' { incr nb_chars ; incr nb_chars ; Timesflt }
 | '/' { incr nb_chars ; Div }
 | '%' { incr nb_chars ; Modulo }
 | '(' { incr nb_chars ; Lparen }
 | ')' { incr nb_chars ; Rparen }
(* | ' ' { incr nb_chars ; token lexbuf } *)
(* | '\n' { incr nb_chars ; token lexbuf } *)
(* | '\t' { incr nb_chars ; token lexbuf } *)
 | 'f' 'l' 'o' 'a' [^ 't']?
 | 'f' 'l' 'o' [^ 'a']? 't'
 | 'f' 'l' [^ 'o']? 'a' 't'
 | 'f' [^ 'l']? 'o' 'a' 't'
 | [^ 'f'] 'l' 'o' 'a' 't' as x
 { raise (Invalid_keyword ("Unexpected word at position " ^ string_of_int (!nb_chars) ^ ": " ^ x ^ "\nDid you mean \"float\"?")) }
 | 'i' 'n' [^ 't']?
 | 'i' [^ 'n']? 't'
 | [^ 'i']? 'n' 't' as x
 { raise (Invalid_keyword ("Unexpected word at position " ^ string_of_int (!nb_chars) ^ ": " ^ x ^ "\nDid you mean \"int\"?\n\n")) }
 | _ as x { raise (Invalid_character ("Unexpected character at position " ^ string_of_int (!nb_chars) ^ ": " ^ String.make 1 x))}
 | '\n' eof (* Had some mistakes triggered by last character being a \n, even though I didn't write any in my .exp file, seems to work with this case handled *)
 | eof { EOF }
 
 {
 }
