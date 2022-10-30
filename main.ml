open Avengers_assemble
open Format
open Lexer_luthorer
open Parser_toutatisser
open Syntaxic_lemon_tree
open X86_64

let main f =

 (* val main: string -> unit
 [main f] ensures that f is of the form pref ^ ".exp", then go through the lex-pars analysis on the content of a hypothetical file named f
 If no error is triggered, it then creates a file named pref ^ ".s" containing X86_64-assembler code corresponding to the content of f
 *)

 let n = String.length f in
 if n < 4 || String.sub f (n-4) 4 <> ".exp"
 then failwith "Name of file is not of the expected format"
 else
 (
  let lexed_f = open_in f in
  let lexbuf = Lexing.from_channel lexed_f in
  let syntre = Parser_toutatisser.line Lexer_luthorer.token lexbuf in
  
  let fs = String.sub f 0 (n-4) ^ ".s" in
  let output_fs = open_out fs in
  let fmt = formatter_of_out_channel output_fs in
  
  print_program fmt (compile syntre);
  close_out output_fs;
 )
;;

main Sys.argv.(1)
;;
