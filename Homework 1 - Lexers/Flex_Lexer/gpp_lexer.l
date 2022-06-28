/**********************************************
  *  Burak Cicek                              *
  *                                           *
  *  CSE 341 - ASSIGNMENT 1 / Part 1          *
  *********************************************/

/*		Deterministic Finite Automata (DFA)
LINE 54	: Regular expression for identifier.
LINE 55	: Regular expression for integer. If your number has more than one digit, it is an integer value.
LINE 56 : Regular expression for digit. If your number has one digit, it is a digit.
LINE 57	: Regular expression for invalid value. If your number has more than one digit but the first digit is 0, it is an invalid value
LINE 58 : Regular expression for comment line. If you commented something after ;; operator, this expression ignores them.
LINE 59-68: Error situations
LINE 69	: Ignores new lines and tabs.
*/


%{
#include <stdio.h>
%}

%%

\** fprintf(yyout, "OP_DBLMULT\n");
\‘ fprintf(yyout, "OP_OC\n");
\’ fprintf(yyout, "OP_CC\n");
\, fprintf(yyout, "OP_COMMA\n");
\+ fprintf(yyout, "OP_PLUS\n");
\- fprintf(yyout, "OP_MINUS\n");
\/	fprintf(yyout, "OP_DIV\n");
\* fprintf(yyout, "OP_MULT\n");
\( fprintf(yyout, "OP_OP\n");
\) fprintf(yyout, "OP_CP\n");


"deffun" fprintf(yyout, "KW_DEFFUN\n");
"for" fprintf(yyout, "KW_FOR\n");
"if" fprintf(yyout, "KW_IF\n");
"exit" fprintf(yyout, "KW_EXIT\n");
"load" fprintf(yyout, "KW_LOAD\n");
"disp" fprintf(yyout, "KW_DISP\n");
"true" fprintf(yyout, "KW_TRUE\n");
"false" fprintf(yyout, "KW_FALSE\n");
"and" fprintf(yyout, "KW_AND\n");
"or" fprintf(yyout, "KW_OR\n");
"not" fprintf(yyout, "KW_NOT\n");
"equal" fprintf(yyout, "KW_EQUAL\n");
"less" fprintf(yyout, "KW_LESS\n");
"nil" fprintf(yyout, "KW_NIL\n");
"list" fprintf(yyout, "KW_LIST\n");
"append" fprintf(yyout, "KW_APPEND\n");
"concat" fprintf(yyout, "KW_CONCAT\n");
"set" fprintf(yyout, "KW_SET\n");


[a-zA-z][a-zA-z0-9]* fprintf(yyout, "IDENTIFIER\n");
[1-9][0-9]* fprintf(yyout, "VALUE\n");
[0-9] fprintf(yyout, "VALUE\n");
[0-9][0-9]* fprintf(yyout, "INVALID VALUE\n");
[;;]+([A-Za-z]|[0-9]|[. - + / * **])*  fprintf(yyout, "COMMENT\n");
[0-9]+[a-zA-z0-9]* fprintf(yyout, "ERROR\n");
[a-zA-Z0-9]*[?]+[a-zA-Z0-9]* fprintf(yyout, "ERROR\n");
[a-zA-Z0-9]*[!]+[a-zA-Z0-9]* fprintf(yyout, "ERROR\n");
[a-zA-Z0-9]*[.]+[a-zA-Z0-9]* fprintf(yyout, "ERROR\n");
[a-zA-Z0-9]*[:]+[a-zA-Z0-9]* fprintf(yyout, "ERROR\n");
[?]+[a-zA-Z]*[a-zA-Z0-9]* fprintf(yyout, "ERROR\n");
[!]+[a-zA-Z]*[a-zA-Z0-9]* fprintf(yyout, "ERROR\n");
[.]+[a-zA-Z]*[a-zA-Z0-9]* fprintf(yyout, "ERROR\n");
[:]+[a-zA-Z]*[a-zA-Z0-9]* fprintf(yyout, "ERROR\n");
[$]+[a-zA-Z]*[a-zA-Z0-9]* fprintf(yyout, "ERROR\n");
[ \n /t]+	;


%%

int yywrap() {
	return 1;
}

int main(int argc, char *argv[]) {

	if(argc == 1) {
		yyout = fopen("parsed_cpp.txt", "w");
		yylex();
	}

	if(argc == 2){
		yyin = fopen(argv[1], "r");
		yyout = fopen("parsed_cpp.txt","w");
		yylex();
	}
	
	fclose(yyout);
	fclose(yyin);

	return 0;
}