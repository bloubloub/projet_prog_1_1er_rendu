open Syntaxic_lemon_tree
open X86_64

(** Definition of portions of text **)

let global = inline "\t.globl main\n\nmain:\n" ++ pushq (reg rbp)
(* It will be the header of every text section *)

let tail_text_int =
popq rsi
++ inline "\tmovq $format, %rdi\n"
++ movq (imm 0) (reg rax)
++ call "printf"
++ popq rbp
++ ret
++ inline "\n"

(* Ends the text section in the case of an int expression *)

let result_fmt_int = inline "format:\n\t.string \"%d\\n\"\n"

(* Used for printing the result in the case of an int expression *)

let tail_text_flt = 
popsd (reg xmm0)
++ inline "\tmovq $format, %rdi\n"
++ movq (imm 1) (reg rax)
++ call "printf"
++ popq rbp
++ ret
++ inline "\n"

(* Ends the text section in the case of an int expression *)

let result_fmt_flt = inline "format:\n\t.string \"%f\\n\"\n"

(* Used for printing the result in the case of an int expression *)

let compile syntre =

 (* val compile: syn_tree -> program = <fun>
 [compile syntre] returns an X86_64-assembler program calculating the arithmetical expression held in [syntre] *)

 let i = ref (-1) in (* Counter of float variables used *)
 let rec constr_txt_dat text data = function
 
  (* val constr_txt_dat = [ `text ] asm -> [ `data ] asm -> syn_tree -> [ `text ] asm * [ `data ] asm * bool = <fun>
  [constr_txt_dat text data syntre] returns the concatenation of [text] (resp. [data]) and the text (resp. data) held in [syntre], as well as a boolean indicating if the final expression is of type integeer (case true) or float (case false) *)
  
  | T_int (x) ->
    text
    ++ movq (imm x) (reg rdi)
    ++ pushq (reg rdi),
    
    data,
    
    true

  | T_multint (e1, e2)
    -> let t1, d1, _ = constr_txt_dat text data e1
    in let  t2, d2, _ = constr_txt_dat t1 d1 e2 in

    t2
    ++ popq rdi
    ++ popq rsi
    ++ imulq (reg rdi) (reg rsi)
    ++ pushq (reg rsi),

    d2,

    true
  
 | T_addint (e1, e2)
    -> let t1, d1, _ = constr_txt_dat text data e1
    in let t2, d2, _ = constr_txt_dat t1 d1 e2 in

    t2
    ++ popq rdi
    ++ popq rsi
    ++ addq (reg rdi) (reg rsi)
    ++ pushq (reg rsi),

    d2,

    true

  | T_subint (e1, e2)
    -> let t1, d1, _ = constr_txt_dat text data e1
    in let t2, d2, _ = constr_txt_dat t1 d1 e2 in

    t2
    ++ popq rsi
    ++ popq rdi
    ++ subq (reg rsi) (reg rdi)
    ++ pushq (reg rdi),

    d2,

    true

  | T_div (e1, e2)
    -> let t1, d1, _ = constr_txt_dat text data e1
    in let t2, d2, _ = constr_txt_dat t1 d1 e2 in

    t2
    ++ popq rdi
    ++ popq rax
    ++ movq (imm 0) (reg rdx)
    ++ idivq (reg rdi)
    ++ pushq (reg rax),

    d2,

    true

 | T_modulo (e1, e2)
    -> let t1, d1, _ = constr_txt_dat text data e1
    in let t2, d2, _ = constr_txt_dat t1 d1 e2 in

    
    t2
    ++ popq rdi
    ++ popq rax
    ++ movq (imm 0) (reg rdx)
    ++ idivq (reg rdi)
    ++ pushq (reg rdx),

    d2,

    true

 | T_convint (e) -> let t1, d1, _ = constr_txt_dat text data e in
    
    t1
    ++ popsd (reg xmm0)
    ++ cvtsd2siq (reg xmm0) (reg rdi)
    ++ pushq (reg rdi),
    
    d1,

    true

 | T_float (x) -> incr i;

    text
    ++ inline ("\tmovsd f" ^ string_of_int (!i) ^ ", %xmm0\n")
    ++ pushsd (reg xmm0),

    data
    ++ inline ("f" ^ string_of_int (!i) ^ ":\n\t.double " ^ string_of_float x ^ " \n"),

    false

 | T_multfloat (e1, e2) ->
    let t1, d1, _ = constr_txt_dat text data e1 in
    let t2, d2, _ = constr_txt_dat t1 d1 e2 in

    t2
    ++ popsd (reg xmm0)
    ++ popsd (reg xmm1)
    ++ mulsd (reg xmm0) (reg xmm1)
    ++ pushsd (reg xmm1),

    d2,

    false

 | T_addfloat (e1, e2) ->
    let t1, d1, _ = constr_txt_dat text data e1 in
    let t2, d2, _ = constr_txt_dat t1 d1 e2 in

    t2
    ++ popsd (reg xmm0)
    ++ popsd (reg xmm1)
    ++ addsd (reg xmm0) (reg xmm1)
    ++ pushsd (reg xmm1),

    d2,

    false

 | T_subfloat (e1, e2) ->
    let t1, d1, _ = constr_txt_dat text data e1 in
    let t2, d2, _ = constr_txt_dat t1 d1 e2 in

    t2
    ++ popsd (reg xmm1)
    ++ popsd (reg xmm0)
    ++ subsd (reg xmm1) (reg xmm0)
    ++ pushsd (reg xmm0),

    d2,

    false

 | T_convfloat (e) ->
    let t, d, _ = constr_txt_dat text data e in

    t
    ++ popq rdi
    ++ cvtsi2sdq (reg rdi) (reg xmm0)
    ++ pushsd (reg xmm0),

    d,

    false

 in let text, data, is_int = constr_txt_dat nop nop syntre
 in let tail_text, result_fmt = if is_int then tail_text_int, result_fmt_int else tail_text_flt, result_fmt_flt
 in
 {
  text =
  global
  ++ text
  ++ tail_text ;

  data = 
  data
  ++ result_fmt
 }
