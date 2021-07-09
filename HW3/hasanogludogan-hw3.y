%{
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
void yyerror (const char *s) 
{
}
int yylex();

typedef struct printNode
{
	struct printNode * next;
	int intnumb;
	double realnumb;
	int linenumb;
	char * strvalue;
	int printtype;
} printNode;

printNode * rootPtr = NULL;
printNode *addmismatch(printNode * head, int lineno, int type);
printNode *addint(printNode * head, int lineno, int num, int type);

printNode *addreal(printNode * head, int lineno, double num, int type);

printNode *addstring(printNode * head, int lineno, char * strval, int type);

void prettyprint(printNode * head);

typedef struct attrNode
{
	int intnum;
	double realnum;
	int lineno;
	char * strval;
	int sonType;
	int type;
} attrNode;
%}

%union {
	char * currstr;
	int linenumber;
	int integer_number;
	double real_number;
	
	struct{
		int intnum;
		double realnum;
		int lineno;
		char * strval;
		int sonType;
		int type;
	}attrNode;
	
}

%token tPRINT tGET tSET tFUNCTION tRETURN tIDENT tEQUALITY tIF tGT tLT tGEQ tLEQ tINC tDEC
%token <linenumber> tADD
%token <linenumber> tSUB
%token <linenumber> tMUL
%token <linenumber> tDIV
%token <integer_number> tINT
%token <real_number> tREAL
%token <currstr> tSTRING
%type <attrNode> operation
%type <attrNode> expr
%type <attrNode> returnStmt
%type <attrNode> setStmt
%type <attrNode> print

%start prog

%%
prog:		'[' stmtlst ']'
;

stmtlst:	stmtlst stmt |
;

stmt:		setStmt 		{
                                                if($1.type == 1)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                               	// printf("Result of expression on %d is (%d)\n", $1.lineno, $1.intnum);
								rootPtr = addint(rootPtr, $1.lineno, $1.intnum, $1.type);
                                                        }
                                                }
                                                else if($1.type == 2)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%.1f)\n", $1.lineno, $1.realnum);
								rootPtr = addreal(rootPtr, $1.lineno, $1.realnum, $1.type);
                                                        }
                                                }
                                                else if($1.type == 3)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%s)\n", $1.lineno, $1.strval);
								rootPtr = addstring(rootPtr, $1.lineno, $1.strval, $1.type);
                                                        }
                                                }
                                        }
		| if 
		| print 		{
                                                if($1.type == 1)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%d)\n", $1.lineno, $1.intnum);
								rootPtr = addint(rootPtr, $1.lineno, $1.intnum, $1.type);
                                                        }
                                                }
                                                else if($1.type == 2)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%.1f)\n", $1.lineno, $1.realnum);
								rootPtr = addreal(rootPtr, $1.lineno, $1.realnum, $1.type);
                                                        }
                                                }
                                                else if($1.type == 3)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%s)\n", $1.lineno, $1.strval);
								rootPtr = addstring(rootPtr, $1.lineno, $1.strval, $1.type);
                                                        }
                                                }
                                        }
		| unaryOperation 
		| expr			{
						if($1.type == 1)
						{
							if($1.sonType == 2)
							{
								//printf("Result of expression on %d is (%d)\n", $1.lineno, $1.intnum);
								rootPtr = addint(rootPtr, $1.lineno, $1.intnum, $1.type);
							}
						}
						else if($1.type == 2)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%.1f)\n", $1.lineno, $1.realnum);
								rootPtr = addreal(rootPtr, $1.lineno, $1.realnum, $1.type);
                                                        }
                                                }
						else if($1.type == 3)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%s)\n", $1.lineno, $1.strval);
								rootPtr = addstring(rootPtr, $1.lineno, $1.strval, $1.type);
                                                        }
                                                }
					} 
		| returnStmt		{
						if($1.type == 1)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%d)\n", $1.lineno, $1.intnum);
								rootPtr = addint(rootPtr, $1.lineno, $1.intnum, $1.type);
                                                        }
                                                }
                                                else if($1.type == 2)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%.1f)\n", $1.lineno, $1.realnum);
								rootPtr = addreal(rootPtr, $1.lineno, $1.realnum, $1.type);
                                                        }
                                                }
                                                else if($1.type == 3)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%s)\n", $1.lineno, $1.strval);
								rootPtr = addstring(rootPtr, $1.lineno, $1.strval, $1.type);
                                                        }
                                                }

					}
;

getExpr:	'[' tGET ',' tIDENT ',' '[' exprList ']' ']'	
		| '[' tGET ',' tIDENT ',' '[' ']' ']'		
		| '[' tGET ',' tIDENT ']'			
;

setStmt:	'[' tSET ',' tIDENT ',' expr ']'	{
								if($6.type == 1)
                                        			{
                                                			$$.intnum = $6.intnum;
                                                			$$.type = 1;
                                                			$$.sonType = $6.sonType;
									if($$.sonType == 2)
                                                                        { 
                                                                                $$.lineno = $6.lineno;
                                                                        }
                                        			}
                                        			else if($6.type == 2)
                                        			{
                                               				$$.realnum = $6.realnum;
                                                			$$.type = 2;
                                                			$$.sonType = $6.sonType;
                                                			if($$.sonType == 2)
                                                                        { 
                                                                                $$.lineno = $6.lineno;
                                                                        }
                                        			}
                                        			else if($6.type == 3)
                                        			{
                                                			$$.strval = $6.strval;
                                                			$$.type = 3;
                                                			$$.sonType = $6.sonType;
									if($$.sonType == 2)
									{
										$$.lineno = $6.lineno;
									}
                                        			}
                                        			else
                                        			{
                                                			$$.type = 4;
                                                			$$.sonType = 1;
                                        			}							
							}
;

if:		'[' tIF ',' condition ',' '[' stmtlst ']' ']'
		| '[' tIF ',' condition ',' '[' stmtlst ']' '[' stmtlst ']' ']'
;

print:		'[' tPRINT ',' '[' expr ']' ']'		{
                                                                if($5.type == 1)
                                                                {
                                                                        $$.intnum = $5.intnum;
                                                                        $$.type = 1;
                                                                        $$.sonType = $5.sonType;
                                                                        if($$.sonType == 2)
                                                                        {
                                                                                $$.lineno = $5.lineno;
                                                                        }
                                                                }
                                                                else if($5.type == 2)
                                                                {
                                                                        $$.realnum = $5.realnum;
                                                                        $$.type = 2;
                                                                        $$.sonType = $5.sonType;
                                                                        if($$.sonType == 2)
                                                                        {
                                                                                $$.lineno = $5.lineno;
                                                                        }
                                                                }
                                                                else if($5.type == 3)
                                                                {
                                                                        $$.strval = $5.strval;
                                                                        $$.type = 3;
                                                                        $$.sonType = $5.sonType;
                                                                        if($$.sonType == 2)
                                                                        {
                                                                                $$.lineno = $5.lineno;
                                                                        }
                                                                }
                                                                else
                                                                {
                                                                        $$.type = 4;
                                                                        $$.sonType = 1;
                                                                }
                                                        }

;

operation:	'[' tADD ',' expr ',' expr ']'		{ 
								if($4.type == 1 && $6.type == 1)
								{
									$$.type = 1;
									$$.lineno = $2;
									$$.intnum = $4.intnum + $6.intnum;
									$$.sonType = 2;
								}
								else if($4.type == 3 && $6.type == 3)
								{
									$$.type = 3;
									$$.lineno = $2;
									char * temp = (char*) malloc(1 + strlen($4.strval) + strlen($6.strval));
									strcpy(temp, $4.strval);
									strcat(temp, $6.strval);
									$$.strval = temp;
									$$.sonType = 2;
								}
								else if($4.type == 2 && $6.type == 2)
								{
									$$.type = 2;
									$$.lineno = $2;
									$$.realnum = $4.realnum + $6.realnum;
									$$.sonType = 2;
								}						
								else if($4.type == 2 && $6.type == 1)
								{
									$$.type = 2;
									$$.lineno = $2;
									$$.realnum = $4.realnum + $6.intnum;
									$$.sonType = 2;
								}
								else if($4.type == 1 && $6.type == 2)
								{
									$$.type = 2;
									$$.lineno = $2;
									$$.realnum = $4.intnum + $6.realnum;
									$$.sonType = 2; 
								}
								else if($4.type == 4 || $6.type == 4)
								{
									$$.type = 4;
									$$.lineno = $2;
									$$.sonType = 1;
								}
								else if($4.type != $6.type)
								{
									$$.type = 4;
									$$.lineno = $2;
									rootPtr = addmismatch(rootPtr, $$.lineno, $$.type);
									//printf("Type mismatch on %d \n", $$.lineno);
									$$.sonType = 1;
								}		
							}
		| '[' tSUB ',' expr ',' expr ']'	{
								if($4.type == 1 && $6.type == 1)
                                                                {
                                                                        $$.type = 1;
                                                                        $$.lineno = $2;
                                                                        $$.intnum = $4.intnum - $6.intnum;
                                                                        $$.sonType = 2;
                                                                }
								else if($4.type == 2 && $6.type == 2)
                                                                {
                                                                        $$.type = 2;
                                                                        $$.lineno = $2;
                                                                        $$.realnum = $4.realnum - $6.realnum;
                                                                        $$.sonType = 2;
                                                                }
                                                                else if($4.type == 2 && $6.type == 1)
                                                                {
                                                                        $$.type = 2;
                                                                        $$.lineno = $2;
                                                                        $$.realnum = $4.realnum - $6.intnum;
                                                                        $$.sonType = 2;
                                                                }
                                                                else if($4.type == 1 && $6.type == 2)
                                                                {
                                                                        $$.type = 2;
                                                                        $$.lineno = $2;
                                                                        $$.realnum = $4.intnum - $6.realnum;
                                                                        $$.sonType = 2;
                                                                }
								else if($4.type == 4 || $6.type == 4)
                                                                {
                                                                        $$.type = 4;
                                                                        $$.lineno = $2;
                                                                        $$.sonType = 1;
                                                                }
								else if($4.type == 3 && $6.type == 3)
                                                                {
                                                                        $$.type = 4;
                                                                        $$.lineno = $2;
									rootPtr = addmismatch(rootPtr, $$.lineno, $$.type );
                                                                        //printf("Type mismatch on %d \n", $$.lineno);
                                                                        $$.sonType = 1;
                                                                }
                                                                else if($4.type != $6.type)
                                                                {
                                                                        $$.type = 4;
                                                                        $$.lineno = $2;
                                                                        rootPtr = addmismatch(rootPtr, $$.lineno, $$.type );
									//printf("Type mismatch on %d \n", $$.lineno);
									$$.sonType = 1;
                                                                }
							}
		| '[' tMUL ',' expr ',' expr ']'	{ 
								if($4.type == 1 && $6.type == 1)
                                                                {
                                                                        $$.type = 1;
                                                                        $$.lineno = $2;
                                                                        $$.intnum = $4.intnum * $6.intnum;
                                                                        $$.sonType = 2;
                                                                }
                                                                else if($4.type == 2 && $6.type == 2)
                                                                {
                                                                        $$.type = 2;
                                                                        $$.lineno = $2;
                                                                        $$.realnum = $4.realnum * $6.realnum;
                                                                        $$.sonType = 2;
                                                                }
                                                                else if($4.type == 2 && $6.type == 1)
                                                                {
                                                                        $$.type = 2;
                                                                        $$.lineno = $2;
                                                                        $$.realnum = $4.realnum * $6.intnum;
                                                                        $$.sonType = 2;
                                                                }
                                                                else if($4.type == 1 && $6.type == 2)
                                                                {
                                                                        $$.type = 2;
                                                                        $$.lineno = $2;
                                                                        $$.realnum = $4.intnum * $6.realnum;
                                                                        $$.sonType = 2;
                                                                }
								else if($4.type == 1  && $6.type == 3)
                                                                {
                                                                        $$.type = 3;
                                                                        $$.lineno = $2;
									if($4.intnum < 0)
									{
										$$.type = 4;
                                                                        	$$.lineno = $2;
                                                                        	//printf("Type mismatch on %d \n", $$.lineno);
                                                                        	rootPtr = addmismatch(rootPtr, $$.lineno, $$.type );
                                                                        	$$.sonType = 1;
									}
									else if($4.intnum == 0)
									{
										char * temp = (char *) malloc(sizeof(char));
										$$.strval = temp;
										$$.sonType = 2;
									}
									else
									{
										char * temp = (char *) malloc(1 + $4.intnum * strlen($6.strval));
										strcpy(temp, $6.strval);
										int i;
										for(i = 1; i < $4.intnum; i = i + 1)
										{
											strcat(temp, $6.strval);
										}
                                                                        	$$.strval = temp;
                                                                        	$$.sonType = 2;
									}
                                                                }
                                                                else if($4.type == 4 || $6.type == 4)
                                                                {
                                                                        $$.type = 4;
                                                                        $$.lineno = $2;
                                                                        $$.sonType = 1;
                                                                }
								else if($4.type == 3 && $6.type == 3)
								{
									$$.type = 4;
                                                                        $$.lineno = $2;
                                                                        //printf("Type mismatch on %d \n", $$.lineno);
									rootPtr = addmismatch(rootPtr, $$.lineno, $$.type );
                                                                        $$.sonType = 1;
								}
                                                                else if($4.type != $6.type)
                                                                {
                                                                        $$.type = 4;
                                                                        $$.lineno = $2;
                                                                        //printf("Type mismatch on %d \n", $$.lineno);
									rootPtr = addmismatch(rootPtr, $$.lineno, $$.type );
									$$.sonType = 1;
                                                                }
							}
		| '[' tDIV ',' expr ',' expr ']'	{ 
								if($4.type == 1 && $6.type == 1)
                                                                {
                                                                        $$.type = 1;
                                                                        $$.lineno = $2;
                                                                        $$.intnum = $4.intnum / $6.intnum;
                                                                        $$.sonType = 2;
                                                                }
                                                                else if($4.type == 2 && $6.type == 2)
                                                                {
                                                                        $$.type = 2;
                                                                        $$.lineno = $2;
                                                                        $$.realnum = $4.realnum / $6.realnum;
                                                                        $$.sonType = 2;
                                                                }
                                                                else if($4.type == 2 && $6.type == 1)
                                                                {
                                                                        $$.type = 2;
                                                                        $$.lineno = $2;
									$$.realnum = $4.realnum / $6.intnum;
                                                                        $$.sonType = 2;
                                                                }
                                                                else if($4.type == 1 && $6.type == 2)
                                                                {
                                                                        $$.type = 2;
                                                                        $$.lineno = $2;
                                                                        $$.realnum = $4.intnum / $6.realnum;
                                                                        $$.sonType = 2;
                                                                }
								else if($4.type == 4 || $6.type == 4)
                                                                {
                                                                        $$.type = 4;
                                                                        $$.lineno = $2;
                                                                        $$.sonType = 1;
                                                                }
								else if($4.type == 3 && $6.type == 3)
                                                                {
                                                                        $$.type = 4;
                                                                        $$.lineno = $2;
									rootPtr = addmismatch(rootPtr, $$.lineno, $$.type );
                                                                        //printf("Type mismatch on %d \n", $$.lineno);
                                                                        $$.sonType = 1;
                                                                }
                                                                else if($4.type != $6.type)
                                                                {
                                                                        $$.type = 4;
                                                                        $$.lineno = $2;
									rootPtr = addmismatch(rootPtr, $$.lineno, $$.type );
                                                                        //printf("Type mismatch on %d \n", $$.lineno);
									$$.sonType = 1;
                                                                }
							}
;	

unaryOperation: '[' tINC ',' tIDENT ']'
		| '[' tDEC ',' tIDENT ']'
;

expr:		tINT 		{
					$$.type = 1;
					$$.sonType = 1;
					$$.intnum = $1;
				}
		| tREAL 	{
					$$.type	= 2;
                                        $$.sonType = 1;
                                        $$.realnum = $1;
				}
		| tSTRING 	{
					$$.type = 3;
                                        $$.sonType = 1;
                                        $$.strval = $1;
				}
		| getExpr 	{
					$$.type = 4;
					$$.sonType = 1;
				} 
		| function 	{
					$$.type = 4;
					$$.sonType = 1;
				} 
		| operation 	{
					if($1.type == 1)
					{
						$$.intnum = $1.intnum;
						$$.type = 1;
						$$.sonType = 2;
						$$.lineno = $1.lineno;
					}
					else if($1.type == 2)
                                        {
                                                $$.realnum = $1.realnum;
                                                $$.type	= 2;
                                                $$.sonType = 2;
                                                $$.lineno = $1.lineno;
                                        }
					else if($1.type == 3)
                                        {
                                                $$.strval = $1.strval;
                                                $$.type = 3;
                                                $$.sonType = 2;
                                                $$.lineno = $1.lineno;
                                        }
					else
					{
						$$.type = 4;
						$$.sonType = 1;
					}
			    	}
		| condition 	{
					$$.type = 4;
					$$.sonType = 1;
				}
;

function:	 '[' tFUNCTION ',' '[' parametersList ']' ',' '[' stmtlst ']' ']' 
		| '[' tFUNCTION ',' '[' ']' ',' '[' stmtlst ']' ']' 
;

condition:	'[' tEQUALITY ',' expr ',' expr ']'	{
								if($4.sonType == 2)
								{
									if($4.type == 1)
									{
										//printf("Result of expression on %d is (%d)\n", $4.lineno, $4.intnum);
										rootPtr = addint(rootPtr, $4.lineno, $4.intnum, $4.type);
									}
									else if($4.type ==2)
									{
										//printf("Result of expression on %d is (%.1f)\n", $4.lineno, $4.realnum);
										rootPtr = addreal(rootPtr, $4.lineno, $4.realnum, $4.type);
									}
									else if($4.type == 3)
									{
										//printf("Result of expression on %d is (%s)\n", $4.lineno, $4.strval);
										rootPtr = addstring(rootPtr, $4.lineno, $4.strval, $4.type);
									}
								}
								if($6.sonType == 2)
                                                                {
                                                                        if($6.type == 1)
                                                                        {
                                                                                //printf("Result of expression on %d is (%d)\n", $6.lineno, $6.intnum);
										rootPtr = addint(rootPtr, $6.lineno, $6.intnum, $6.type);
                                                                        }
                                                                        else if($6.type ==2)
                                                                        {
                                                                                //printf("Result of expression on %d is (%.1f)\n", $6.lineno, $6.realnum);
										rootPtr = addreal(rootPtr, $6.lineno, $6.realnum, $6.type);
                                                                        }
                                                                        else if($6.type == 3)
                                                                        {
                                                                                //printf("Result of expression on %d is (%s)\n", $6.lineno, $6.strval);
										rootPtr = addstring(rootPtr, $6.lineno, $6.strval, $6.type);
                                                                        }
                                                                }
							}
		| '[' tGT ',' expr ',' expr ']'		{
                                                                if($4.sonType == 2)
                                                                {
                                                                        if($4.type == 1)
                                                                        {
                                                                                //printf("Result of expression on %d is (%d)\n", $4.lineno, $4.intnum);
										rootPtr = addint(rootPtr, $4.lineno, $4.intnum, $4.type);
                                                                        }
                                                                        else if($4.type ==2)
                                                                        {
                                                                                //printf("Result of expression on %d is (%.1f)\n", $4.lineno, $4.realnum);
										rootPtr = addreal(rootPtr, $4.lineno, $4.realnum, $4.type);
                                                                        }
                                                                        else if($4.type == 3)
                                                                        {
                                                                                //printf("Result of expression on %d is (%s)\n", $4.lineno, $4.strval);
										rootPtr = addstring(rootPtr, $4.lineno, $4.strval, $4.type);
                                                                        }
                                                                }
                                                                if($6.sonType == 2)
                                                                {
                                                                        if($6.type == 1)
                                                                        {
                                                                                //printf("Result of expression on %d is (%d)\n", $6.lineno, $6.intnum);
										rootPtr = addint(rootPtr, $6.lineno, $6.intnum, $6.type);
                                                                        }
                                                                        else if($6.type ==2)
                                                                        {
                                                                                //printf("Result of expression on %d is (%.1f)\n", $6.lineno, $6.realnum);
										rootPtr = addreal(rootPtr, $6.lineno, $6.realnum, $6.type);
                                                                        }
                                                                        else if($6.type == 3)
                                                                        {
                                                                                //printf("Result of expression on %d is (%s)\n", $6.lineno, $6.strval);
										rootPtr = addstring(rootPtr, $6.lineno, $6.strval, $6.type);
                                                                        }
                                                                }
                                                        }
		| '[' tLT ',' expr ',' expr ']'		{
                                                                if($4.sonType == 2)
                                                                {
                                                                        if($4.type == 1)
                                                                        {
                                                                                rootPtr = addint(rootPtr, $4.lineno, $4.intnum, $4.type);
										//printf("Result of expression on %d is (%d)\n", $4.lineno, $4.intnum);
                                                                        }
                                                                        else if($4.type ==2)
                                                                        {
                                                                                rootPtr = addreal(rootPtr, $4.lineno, $4.realnum, $4.type);
										//printf("Result of expression on %d is (%.1f)\n", $4.lineno, $4.realnum);
                                                                        }
                                                                        else if($4.type == 3)
                                                                        {
                                                                                rootPtr = addstring(rootPtr, $4.lineno, $4.strval, $4.type);
										//printf("Result of expression on %d is (%s)\n", $4.lineno, $4.strval);
                                                                        }
                                                                }
                                                                if($6.sonType == 2)
                                                                {
                                                                        if($6.type == 1)
                                                                        {
                                                                                rootPtr = addint(rootPtr, $6.lineno, $6.intnum, $6.type);
										//printf("Result of expression on %d is (%d)\n", $6.lineno, $6.intnum);
                                                                        }
                                                                        else if($6.type ==2)
                                                                        {
                                                                                rootPtr = addreal(rootPtr, $6.lineno, $6.realnum, $6.type);
										//printf("Result of expression on %d is (%.1f)\n", $6.lineno, $6.realnum);
                                                                        }
                                                                        else if($6.type == 3)
                                                                        {
                                                                                rootPtr = addstring(rootPtr, $6.lineno, $6.strval, $6.type);
										//printf("Result of expression on %d is (%s)\n", $6.lineno, $6.strval);
                                                                        }
                                                                }
                                                        }
		| '[' tGEQ ',' expr ',' expr ']'	{
                                                                if($4.sonType == 2)
                                                                {
                                                                        if($4.type == 1)
                                                                        {
                                                                                rootPtr = addint(rootPtr, $4.lineno, $4.intnum, $4.type);
										//printf("Result of expression on %d is (%d)\n", $4.lineno, $4.intnum);
                                                                        }
                                                                        else if($4.type ==2)
                                                                        {
                                                                                //printf("Result of expression on %d is (%.1f)\n", $4.lineno, $4.realnum);
										rootPtr = addreal(rootPtr, $4.lineno, $4.realnum, $4.type);
                                                                        }
                                                                        else if($4.type == 3)
                                                                        {
                                                                                rootPtr = addstring(rootPtr, $4.lineno, $4.strval, $4.type);
										//printf("Result of expression on %d is (%s)\n", $4.lineno, $4.strval);
                                                                        }
                                                                }
                                                                if($6.sonType == 2)
                                                                {
                                                                        if($6.type == 1)
                                                                        {
                                                                                rootPtr = addint(rootPtr, $4.lineno, $4.intnum, $4.type);
										//printf("Result of expression on %d is (%d)\n", $6.lineno, $6.intnum);
                                                                        }
                                                                        else if($6.type == 2)
                                                                        {
                                                                                rootPtr = addreal(rootPtr, $6.lineno, $6.realnum, $6.type);
										//printf("Result of expression on %d is (%.1f)\n", $6.lineno, $6.realnum);
                                                                        }
                                                                        else if($6.type == 3)
                                                                        {
                                                                                rootPtr = addstring(rootPtr, $6.lineno, $6.strval, $6.type);
										//printf("Result of expression on %d is (%s)\n", $6.lineno, $6.strval);
                                                                        }
                                                                }
                                                        }
		| '[' tLEQ ',' expr ',' expr ']'	{
                                                                if($4.sonType == 2)
                                                                {
                                                                        if($4.type == 1)
                                                                        {
                                                                                //printf("Result of expression on %d is (%d)\n", $4.lineno, $4.intnum);
										rootPtr = addint(rootPtr, $4.lineno, $4.intnum, $4.type);
                                                                        }
                                                                        else if($4.type ==2)
                                                                        {
                                                                                //printf("Result of expression on %d is (%.1f)\n", $4.lineno, $4.realnum);
										rootPtr = addreal(rootPtr, $4.lineno, $4.realnum, $4.type);
                                                                        }
                                                                        else if($4.type == 3)
                                                                        {
                                                                                //printf("Result of expression on %d is (%s)\n", $4.lineno, $4.strval);
										rootPtr = addstring(rootPtr, $4.lineno, $4.strval, $4.type);
                                                                        }
                                                                }
                                                                if($6.sonType == 2)
                                                                {
                                                                        if($6.type == 1)
                                                                        {
                                                                                rootPtr = addint(rootPtr, $6.lineno, $6.intnum, $6.type);
										//printf("Result of expression on %d is (%d)\n", $6.lineno, $6.intnum);
                                                                        }
                                                                        else if($6.type ==2)
                                                                        {
                                                                                rootPtr = addreal(rootPtr, $6.lineno, $6.realnum, $6.type);
										//printf("Result of expression on %d is (%.1f)\n", $6.lineno, $6.realnum);
                                                                        }
                                                                        else if($6.type == 3)
                                                                        {
                                                                                rootPtr = addstring(rootPtr, $6.lineno, $6.strval, $6.type);
										//printf("Result of expression on %d is (%s)\n", $6.lineno, $6.strval);
                                                                        }
                                                                }
                                                        }
;

returnStmt:	'[' tRETURN ',' expr ']'	{
							if($4.type == 1)
                                                        {
                                                                $$.intnum = $4.intnum;
                                                                $$.type = 1;
                                                                $$.sonType = $4.sonType;
                                                                if($$.sonType == 2)
                                                                {
                                                                        $$.lineno = $4.lineno;
                                                        	}
                                                        }
                                                        else if($4.type == 2)
                                                        {
                                                                $$.realnum = $4.realnum;
                                                                $$.type = 2;
                                                                $$.sonType = $4.sonType;
                                                                if($$.sonType == 2)
                                                                {
                                                                	$$.lineno = $4.lineno;
                                                                }
                                                        }
                                                    	else if($4.type == 3)
                                                        {
                                                                $$.strval = $4.strval;
                                                                $$.type = 3;
                                                                $$.sonType = $4.sonType;
                                                                if($$.sonType == 2)
                                                                {
                                                                        $$.lineno = $4.lineno;
                                                        	}
                                                        }
                                                        else
                                                        {
                                                                $$.type = 4;
                                                                $$.sonType = 1;
                                                	}
						}
		| '[' tRETURN ']'		{
							$$.type = 4;
							$$.type = 1;
						}
;

parametersList: parametersList ',' tIDENT | tIDENT
;

exprList:	exprList ',' expr	{
						if($3.type == 1)
                                                {
                                                        if($3.sonType == 2)
                                                        {
                                                                rootPtr = addint(rootPtr, $3.lineno, $3.intnum, $3.type);
								//printf("Result of expression on %d is (%d)\n", $3.lineno, $3.intnum);
                                                        }
                                                }
                                                else if($3.type == 2)
                                                {
                                                        if($3.sonType == 2)
                                                        {
                                                                rootPtr = addreal(rootPtr, $3.lineno, $3.realnum, $3.type);
								//printf("Result of expression on %d is (%.1f)\n", $3.lineno, $3.realnum);
                                                        }
                                                }
                                                else if($3.type == 3)
                                                {
                                                        if($3.sonType == 2)
                                                        {
                                                                rootPtr = addstring(rootPtr, $3.lineno, $3.strval, $3.type);
								//printf("Result of expression on %d is (%s)\n", $3.lineno, $3.strval);
                                                   	}
                                                }
					} 
		| expr			{
						if($1.type == 1)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                rootPtr = addint(rootPtr, $1.lineno, $1.intnum, $1.type);
								//printf("Result of expression on %d is (%d)\n", $1.lineno, $1.intnum);
                                                        }
                                                }
                                                else if($1.type == 2)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                rootPtr = addreal(rootPtr, $1.lineno, $1.realnum, $1.type);
								//printf("Result of expression on %d is (%.1f)\n", $1.lineno, $1.realnum);
                                                        }
                                                }
                                                else if($1.type == 3)
                                                {
                                                        if($1.sonType == 2)
                                                        {
                                                                //printf("Result of expression on %d is (%s)\n", $1.lineno, $1.strval);
								rootPtr = addstring(rootPtr, $1.lineno, $1.strval, $1.type);
                                                   	}
						}
					}
;

%%
printNode *addmismatch(printNode * head, int lineno, int type)
{
        if(head == NULL)
        {
                printNode * temp =  (struct printNode *) malloc(sizeof(struct printNode));
                temp->next = NULL;
                temp->linenumb = lineno;
                temp->printtype = type;
                head = temp;
                return head;
        }
	printNode * temp = head;
        while(temp != NULL && temp->next != NULL )
        {
                temp = temp->next;
        }
	printNode * ptr = (struct printNode*) malloc(sizeof(struct printNode));
        ptr->next = NULL;
        ptr->linenumb = lineno;
        ptr->printtype = type;
        temp->next = ptr;
        return head;
}
printNode *addint(printNode * head, int lineno, int num, int type)
{
        if(head == NULL)
        {
                printNode * temp = (struct printNode*) malloc(sizeof(struct printNode));
                temp->next = NULL;
                temp->linenumb = lineno;
                temp->intnumb = num;
                temp->printtype = type;
                head = temp;
                return head;
        }
	printNode * temp = head;
        while(temp != NULL && temp->next != NULL )
        {
                temp = temp->next;
        }
	printNode * ptr = (struct printNode*) malloc(sizeof(struct printNode));
        ptr->next = NULL;
        ptr->linenumb = lineno;
        ptr->intnumb = num;
        ptr->printtype = type;
        temp->next = ptr;
        return head;
}
printNode *addreal(printNode * head, int lineno, double num, int type)
{
        if(head == NULL)
        {
                printNode * temp = (struct printNode*) malloc(sizeof(struct printNode));
                temp->next = NULL;
                temp->linenumb = lineno;
                temp->realnumb = num;
                temp->printtype = type;
                head = temp;
                return head;
        }
	printNode * temp = head;
        while(temp != NULL && temp->next != NULL )
        {
                temp = temp->next;
        }
	printNode * ptr = (struct printNode*) malloc(sizeof( struct printNode));
        ptr->next = NULL;
        ptr->linenumb = lineno;
        ptr->realnumb = num;
        ptr->printtype = type;
        temp->next = ptr;
        return head;
}
printNode *addstring(printNode * head, int lineno, char * strval, int type)
{
        if(head == NULL)
        {
                printNode * temp = (struct printNode*) malloc(sizeof(struct printNode));
                temp->next = NULL;
                temp->linenumb = lineno;
                temp->strvalue = strval;
                temp->printtype = type;
                head = temp;
                return head; 
        }
	printNode * temp = head;
        while(temp != NULL && temp->next != NULL )
        {
                temp = temp->next;
        }
	printNode * ptr = (struct printNode*) malloc(sizeof(struct printNode));
        ptr->next = NULL;
        ptr->linenumb = lineno;
        ptr->strvalue = strval;
        ptr->printtype = type;
        temp->next = ptr;
        return head;
}
void prettyprint(printNode * head)
{
        if(head == NULL)
        {
                return;
        }
	printNode * ptr = head;
        while(ptr != NULL)
        {
                if(ptr->printtype == 1)
                {
                        printf("Result of expression on %d is (%d)\n", ptr->linenumb, ptr->intnumb);
                }
                else if(ptr->printtype == 2)
                {
			printf("Result of expression on %d is (%.1lf)\n", ptr->linenumb, ptr->realnumb);
                }
                else if(ptr->printtype == 3)
                {
                        printf("Result of expression on %d is (%s)\n", ptr->linenumb, ptr->strvalue);
                }
                else
                {
                        printf("Type mismatch on %d\n", ptr->linenumb);
                }
                ptr = ptr->next;

        }
}
int main ()
{
	if (yyparse())
	{
		// parse error
		printf("ERROR\n");
		return 1;
	}
	else
	{
		// successful parsing
		prettyprint(rootPtr);
		return 0;
	}
}
