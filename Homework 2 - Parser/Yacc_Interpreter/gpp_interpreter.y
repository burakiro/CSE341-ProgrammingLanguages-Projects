%{ 
#include<stdio.h> 
#include<math.h>
FILE *yyin;
int flag=0,islst=0,binval=0,indx=0,indxh=0,ext=0,count=0;
int lst[1000];
int yylex(); 
int yyparse();
%} 

%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_DEFVAR KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE KW_WHILE
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_DBLMULT OP_OC OP_CC OP_OP OP_CP OP_COMMA OP_LST 
%token FLE IDENTIFIER COMMENT VALUE FLOAT_VALUE EMPTYSTR
%%

START: | STMT     { 
                  if (flag!=1 && islst==1){
                    printf("Valid\nResult:");
                    printl(lst,indx);  
                    indx=0; indxh=0; islst=0;
                  } else if (flag!=1 && binval==1){
                    printf("Valid\n");
                    if($$==1) printf("Result:T",$$);
                    else printf("Result:F",$$);
                    binval=0;
                  } else if (flag!=1){ printf("Valid\n");
                                       printf("Result:%d",$$); }
                  printf("\n------------------------------------\n");
                  
                  return 0;
                };

STMT: EXPB | EXPLST | EXPA ;


EXPB: OP_OP OP_PLUS EXPB EXPB OP_CP         {$$ = $3 + $4;}
    | OP_OP OP_MINUS EXPB EXPB OP_CP        {$$ = $3 - $4;}
    | OP_OP OP_MULT EXPB EXPB OP_CP         {$$ = $3 * $4;}
    | OP_OP OP_DBLMULT EXPB EXPB OP_CP      {$$ = pow($3,$4);}
    //| OP_OP IDENTIFIER EXPLST OP_CP       {$$=$3; islst=1;}
    | OP_OP KW_DEFVAR IDENTIFIER EXPB OP_CP {$$=$4;}
    | OP_OP KW_SET IDENTIFIER EXPB OP_CP    {$$=$4;}
    | OP_OP IDENTIFIER EXPLST OP_CP         {$$=$3; islst=1;}
    | OP_OP KW_DEFFUN IDENTIFIER LSTI EXPLST OP_CP {$$=$5; islst=1;}
    | OP_OP KW_FOR OP_OP IDENTIFIER EXPB EXPB OP_CP EXPLST OP_CP {islst=1;}
    | OP_OP KW_LIST VAL OP_CP {$$=1; islst=1;}
    | OP_OP KW_EXIT OP_CP {ext=1; printf("exit\n"); return 0;}
    | EMPTYSTR {ext=1; return 0;}
    | OP_OP KW_LOAD OP_OC FLE OP_CC OP_CP {$$=1;}
    | OP_OP KW_DISP EXPB OP_CP {$$=$3; binval=0;}
    | OP_OP KW_IF EXPA EXPLST OP_CP  {$$=$3; if($3 != 1){ indx=0; lst[0]=NULL;} islst = 1;}
    | OP_OP OP_DIV EXPB EXPB OP_CP   { if ($4==0) yyerror();  else{$$ = $3 / $4;} }
    | OP_OP KW_IF EXPA EXPLST EXPLST OP_CP {$$=$3; if($3==1) indx = indxh; 
    											   else{
                                                    indx = indx - indxh;
                                                    for(int i=0; i< indx; i++){
                                                     lst[i] = lst[indxh+i];
                                                    }}
                                                   islst=1;}
    | OP_OP KW_WHILE EXPA EXPLST OP_CP {$$=$3;if($3!=1){indx=0;lst[0]=NULL;}islst=1;}
    | IDENTIFIER{$$=1;}
    | VALUE{$$=$1;}
    | COMMENT{ printf("COMMENT\n------------------------------------\n"); return 0;};

EXPA: OP_OP KW_AND EXPA EXPA OP_CP      {$$ = $3 && $4; binval=1;}
    | OP_OP KW_LESS EXPB EXPB OP_CP     {$$=($3<$4); binval=1;}
    | OP_OP KW_OR EXPA EXPA OP_CP       {$$ = $3 || $4; binval=1;}
    | OP_OP KW_EQUAL EXPB EXPB OP_CP    {$$=($3==$4); binval=1;}
    | OP_OP KW_NOT EXPA OP_CP           {$$ =! $3; binval=1;}
    | OP_OP KW_EQUAL EXPA EXPA OP_CP    {$$=($3==$4); binval=1;}
    | KW_TRUE                           { $$=1; binval=1;}
    | KW_FALSE                          { $$=0; binval=1;};
EXPLST: OP_OP KW_CONCAT EXPLST EXPLST OP_CP {$$=1; islst=1;}
      | OP_OP KW_APPEND EXPB EXPLST OP_CP {$$=1; 
        									for(int i=indx; i>=0; i--){
										        lst[i+1]=lst[i];
										    }
										    lst[0]=$3;
        									indx++; islst=1;}
      | OP_LST VAL OP_CP {islst=1; if(indxh==0) indxh=indx; }
      | OP_LST OP_CP {$$=0; islst=1; indx=0;}
      | KW_NIL{$$=0;};
LSTI: OP_OP LST OP_CP;
LST: LST IDENTIFIER | IDENTIFIER;

VAL: VAL VALUE {lst[indx]=$2; indx++;}
   | VALUE {lst[indx]=$1; indx++;}
   | KW_NIL {$$=0;};
%%

int printl(int list[], int size){
    printf("(");
    for(int i=0;i<size;i++){
        if(i==size-1) printf("%d",list[i]);
        else printf("%d ",list[i]);
    }printf(")");
}
int yyerror(){
    printf("\nSYNTAX_ERROR\n"); 
    flag=1; ext=1; exit(0);
}

int main(int argc, char *argv[]){
    FILE *fp;
    char filename[50];
    if(argc==1){
    	printf("Start writing your codes. Empty string will terminate the REPL mode\n");
        printf("--------------------------------------------------------------------\n");
        yyin=stdin;
        while(!ext){  
            yyparse();
        }


    }
    else if(argc==2){
        fp = fopen(argv[1],"r"); 
        yyin=fp;
        while(!ext)
            yyparse();
    }

    else{ printf("Your command is not correct!");}

    return 0;
}

