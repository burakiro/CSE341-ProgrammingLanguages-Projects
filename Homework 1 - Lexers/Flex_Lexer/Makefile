all: gpp_lexer.c
	gcc -o gpp_lexer.out gpp_lexer.c

lex.yy.c: gpp_lexer.l
	lex -o gpp_lexer.c gpp_lexer.l
	
clean:
	rm a.out gpp_lexer.c