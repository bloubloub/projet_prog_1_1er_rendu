(* Here we define the type of the structure we'll be working on during the translation in Assembly*)

type syn_tree =
 T_float of float
 | T_int of int
 | T_addfloat of syn_tree * syn_tree
 | T_addint of syn_tree * syn_tree
 | T_convfloat of syn_tree
 | T_convint of syn_tree
 | T_div of syn_tree * syn_tree
 | T_modulo of syn_tree * syn_tree
 | T_multfloat of syn_tree * syn_tree
 | T_multint of syn_tree * syn_tree
 | T_subfloat of syn_tree * syn_tree
 | T_subint of syn_tree * syn_tree
 
 
let rec print_syntre = function
 (* val print_syntre : syn_tree -> unit = <fun> 
 [print_syntre t] will print [t]
 Not useful in the main program, but used it to find bugs in my work, so I let it there*)
 
 T_float x -> print_string (string_of_float x);
 
 | T_int x -> print_string (string_of_int x);
 
 | T_addfloat (t1,t2) 
 -> print_string "+F ( ";
 print_syntre t1 ;
 print_string " , " ;
 print_syntre t2 ;
 print_string " )" ;
 
 | T_addint (t1,t2) 
 -> print_string "+I ( ";
 print_syntre t1 ;
 print_string " , " ;
 print_syntre t2 ;
 print_string " )" ;
 
 | T_convfloat t
 -> print_string "Float (" ;
 print_syntre t ;
 print_string " )" ;
 
 | T_convint t
 -> print_string "Int (" ;
 print_syntre t ;
 print_string ")" ;
 
 | T_div (t1,t2) 
 -> print_string "/ ( ";
 print_syntre t1 ;
 print_string " , " ;
 print_syntre t2 ;
 print_string " )" ;

 | T_modulo (t1,t2) 
 -> print_string "% ( ";
 print_syntre t1 ;
 print_string " , " ;
 print_syntre t2 ;
 print_string " )" ;
 
 | T_multfloat (t1,t2) 
 -> print_string "*F ( ";
 print_syntre t1 ;
 print_string " , " ;
 print_syntre t2 ;
 print_string " )" ;
 
 | T_multint (t1,t2) 
 -> print_string "*I ( ";
 print_syntre t1 ;
 print_string " , " ;
 print_syntre t2 ;
 print_string " )" ;
 
 | T_subfloat (t1,t2) 
 -> print_string "-F ( ";
 print_syntre t1 ;
 print_string " , " ;
 print_syntre t2 ;
 print_string " )" ;
 
 | T_subint (t1,t2) 
 -> print_string "-I ( ";
 print_syntre t1 ;
 print_string " , " ;
 print_syntre t2 ;
 print_string " )" ;
