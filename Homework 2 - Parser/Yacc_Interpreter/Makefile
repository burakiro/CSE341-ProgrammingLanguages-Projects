a.out: y.tab.c
	gcc gpp_interpreter.c y.tab.c -w -lm

y.tab.c: gpp_interpreter.c
	yacc -d gpp_interpreter.y

gpp_interpreter.c: gpp_interpreter.l
	lex -o gpp_interpreter.c gpp_interpreter.l

clean:
	rm a.out y.tab.c y.tab.h gpp_interpreter.c 

 