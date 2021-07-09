%{
#include <stdio.h>

void yyerror(const char * msg)
{
	printf("%s\n",msg);
}

%}

%token tSTRING
%token tNUM
%token tPRINT
%token tGET
%token tSET
%token tFUNCTION
%token tRETURN
%token tIDENT
%token tEQUALITY
%token tIF
%token tGT
%token tLT
%token tGEQ
%token tLEQ
%token tINC
%token tDEC

%%

prog:   '[' stmtlst ']'
        | '[' ']'
;

stmtlst:  stmt
        | stmt stmtlst
;

stmt:    set
        | if
        | print
        | increment
        | decrement
        | return
        | expr
;

set:    '[' tSET ',' tIDENT ',' expr ']'
;

if:     '[' tIF ',' condition ',' then ']'
	| '[' tIF ',' condition ',' then else ']'
;

then:    '[' stmtlst ']'
        | '[' ']'
;

else:    '[' stmtlst ']'
        | '[' ']'
;

print:  '[' tPRINT ',' '[' expr ']' ']'
;

increment:      '[' tINC ',' tIDENT ']'
;


decrement:      '[' tDEC ',' tIDENT ']'
;


condition:       '[' tEQUALITY ',' expr ',' expr ']'
                | '[' tGT ',' expr ',' expr ']'
                | '[' tLT ',' expr ',' expr ']'
                | '[' tGEQ ',' expr ',' expr ']'
                | '[' tLEQ ',' expr ',' expr ']'
;


expr:    tNUM
        | tSTRING
        | get
        | function
        | operator
        | condition
;


get:	 '[' tGET ',' tIDENT ']'
	| '[' tGET ',' tIDENT ',' '[' vars ']' ']'
;

vars:	 expr ',' vars
	| expr
	| 
;

function:	 '[' tFUNCTION ',' '[' parameter ']' ',' '[' ']' ']'
		| '[' tFUNCTION ',' '[' parameter ']' ',' '[' stmtlst ']' ']'
;

parameter:       parameter ',' tIDENT
		| tIDENT 
		| 
;

operator:	 '[' '"' '+' '"' ',' expr ',' expr ']'
		|  '[' '"' '-' '"' ',' expr ',' expr ']'
		|  '[' '"' '*' '"' ',' expr ',' expr ']'
		|  '[' '"' '/' '"' ',' expr ',' expr ']'
;

return:  '[' tRETURN ']'
        | '[' tRETURN ',' expr ']'
;


%%


int main ()
{
	if (yyparse())
	{
		printf("ERROR\n");
		return 1;
	}
	else
	{	
		printf("OK\n");
		return 0;
	}
}


