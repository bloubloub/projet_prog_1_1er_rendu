all: aritha rapport.pdf

aritha: lexer_luthorer.ml parser_toutatisser.ml x86_64.mli x86_64.ml syntaxic_lemon_tree.ml avengers_assemble.ml main.ml
	ocamlopt x86_64.mli x86_64.ml syntaxic_lemon_tree.ml parser_toutatisser.mli parser_toutatisser.ml lexer_luthorer.ml avengers_assemble.ml main.ml -o aritha

lexer_luthorer.ml: lexer_luthorer.mll
	ocamllex lexer_luthorer.mll

parser_toutatisser.ml: parser_toutatisser.mly
	ocamlyacc parser_toutatisser.mly

rapport.pdf: rapport.tex
	pdflatex rapport.tex

clean:
	rm -rf *.cmi *.cmx *~ *.o *.s aritha lexer_luthorer.ml parser_toutatisser.ml parser_toutatisser.mli *.log *.aux rapport.pdf
