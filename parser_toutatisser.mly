/* The parser used in main.ml, ensures the given file follows the correct grammatical rules
*/

%{
 open Syntaxic_lemon_tree
%}

%token <float> Flt
%token <int> Integ
%token Convflt Convint Lparen Minusint Minusflt Plusint Plusflt Rparen Timesint Timesflt Div Modulo EOF

%left Minusflt Minusint Plusflt Plusint
%left Div Modulo Timesflt Timesint
%left Convflt Convint

%start line
%type <Syntaxic_lemon_tree.syn_tree> line intexp fltexp

/* A distinction is made between float expressions and int expressions, so that it's easier to keep track of compatibility in the operations.
x +. y with either x or y an integeer expression will ensure a parsing error, for example.
*/

%%line:
 fltexp EOF { $1 }
 | intexp EOF { $1 }
;

intexp:
 Integ { T_int $1 }
 | Plusint Integ { T_int $2 }
 | Minusint Integ { T_subint (T_int 0, T_int $2)}
 | Lparen intexp Rparen { $2 }
 | Plusint Lparen intexp Rparen { $3 }
 | Minusint Lparen intexp Rparen { T_subint (T_int 0, $3)}
 | intexp Timesint intexp { T_multint ($1, $3) }
 | intexp Plusint intexp { T_addint ($1, $3) }
 | intexp Minusint intexp { T_subint ($1, $3) }
 | Convint Lparen fltexp Rparen { T_convint $3 }
 | intexp Div intexp { T_div ($1, $3) }
 | intexp Modulo intexp { T_modulo ($1, $3) }
;

fltexp:
 Flt { T_float $1 }
 | Plusint Flt { T_float $2 }
 | Minusint Flt { T_subfloat (T_float 0.0, T_float $2)}
 | Lparen fltexp Rparen { $2 }
 | Plusint Lparen fltexp Rparen { $3 }
 | Minusint Lparen fltexp Rparen { T_subfloat (T_float 0.0, $3)}
 | fltexp Timesflt fltexp { T_multfloat ($1, $3) }
 | fltexp Plusflt fltexp { T_addfloat ($1, $3) }
 | fltexp Minusflt fltexp { T_subfloat ($1, $3) }
 | Convflt Lparen intexp Rparen { T_convfloat $3 }
;
