%{
	#include "y.tab.h"
	#include <math.h>
	yylval;
%}

%%
[a][n][d]				{return KW_AND;}
[o][r]					{return KW_OR;}
[n][o][t]				{return KW_NOT;}
[e][q][u][a][l]			{return KW_EQUAL;} 
[l][e][s][s]			{return KW_LESS;}
[d][e][f][f][u][n]		{return KW_DEFFUN;}
[f][o][r]				{return KW_FOR;}
[i][f]					{return KW_IF;}
[e][x][i][t]			{return KW_EXIT;}
[l][o][a][d]			{return KW_LOAD;}
[n][i][l]				{return KW_NIL;}
[l][i][s][t]			{return KW_LIST;}
[a][p][p][e][n][d]		{return KW_APPEND;}
[c][o][n][c][a][t]		{return KW_CONCAT;}
[s][e][t]				{return KW_SET;}
[d][i][s][p]		 	{return KW_DISP;}
[t][r][u][e]			{return KW_TRUE;}
[f][a][l][s][e]			{return KW_FALSE;}
[d][e][f][v][a][r] 		{return KW_DEFVAR;}
[w][h][i][l][e]			{return KW_WHILE;}
[a-zA-Z][a-zA-Z0-9]*	{return IDENTIFIER;}
['][(]					{return OP_LST;}
[,]						{return OP_COMMA;}
[“]						{return OP_OC;}
[”]						{return OP_CC;}
[(] 					{return OP_OP;}
[)]						{return OP_CP;}
[+]						{return OP_PLUS;}
[-]						{return OP_MINUS;}
[/]						{return OP_DIV;}
[*]						{return OP_MULT;}
[*][*]					{return OP_DBLMULT;} 
[;][;][a-zA-Z0-9.,-_*?\}=)({/&%$^#'!"+ ]*	{return COMMENT;}
[ \t]+					{;}
[1-9][0-9]*|"0"			{yylval=atoi(yytext); return VALUE;}
[0-9]+"."[0-9]* 		{yylval=atof(yytext);return FLOAT_VALUE;}
[a-zA-Z0-9]+"."[a-z]+	{return FLE;}
[\n][\n]				{return EMPTYSTR;}
%%

int yywrap() 
{ 
return 1; 
} 

