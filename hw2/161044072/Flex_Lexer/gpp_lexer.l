%{
#include <stdio.h>
#include <string.h>
#include "tokens.h"

char errorText[100]; // A string for error messages
int cr=1; // A variable for quotes
%}

%%
[aA][nN][dD]				return AND;
[oO][rR]				return OR;
[nN][oO][tT]				return NOT;
[eE][qQ][uU][aA][lL] 			return EQUAL;
[lL][eE][sS][sS]			return LESS;
[nN][iI][lL]				return NIL;
[lL][iI][sS][tT]			return LIST;
[aA][pP][pP][eE][nN][dD]		return APPEND;
[cC][oO][nN][cC][aA][tT]		return CONCAT;	//Keywords
[sS][eE][tT]				return SET;
[dD][eE][fF][fF][uU][nN]		return DEFFUN;
[fF][oO][rR]				return FOR;
[iI][fF]				return IF;
[eE][xX][iI][tT]			return EXIT;
[lL][oO][aA][dD]			return LOAD;
[dD][iI][sS][pP]			return DISP;
[tT][rR][uU][eE]			return TRUE;
[fF][aA][lL][sS][eE]			return FALSE;
[+] 					return PLUS;
[-] 					return MINUS;
[/] 					return DIV;
[*][*] 					return DBLMULT;	//Operators
[*] 					return MULT;
[(] 					return OP;
[)] 					return CP;
[,]					return COMMA;
["]    					{
						cr++;
						if(cr%2==0)
							return OC;
						else
							return CC;
					
					}

[a-zA-Z_][a-zA-Z0-9_]*			return IDENTIFIER;	
[0]|[1-9][0-9]*				return VALUE;	
[-][1-9][0-9]*				return VALUE;

[\n]					return NEWLINE;	   

[;][;].* 					{
						int cmmnt = 0;
						while(cmmnt != NEWLINE)
							cmmnt = yylex();
						return COMMENT;
					}

							/*Error conditions:*/

[0]|[1-9][0-9]*[+-/*]			{
						strcpy(errorText, yytext);
						return ERROR;
					}
[0][0-9][0-9]* 				{
						strcpy(errorText, yytext);
						return ERROR;
					}

[0-9][a-zA-Z0-9]*			{
						strcpy(errorText, yytext);
						return ERROR;
					}

[+/*,-][^"\t""\n"" "";;"")""("]+		{
						strcpy(errorText, yytext);
						return ERROR;
					}


[a-zA-Z_][a-zA-Z0-9_]*[+*/,.-]		{
						strcpy(errorText, yytext);
						return ERROR;
					}


[\n][\n]				return END;
[ \t]+					return WHITESPACE;

				
%%

void writeIntoFile(FILE *fp, char write[])
{
    
    fputs(write,fp); //Writes given string into given file

}


int yywrap(){} 

int main(int argc, char **argv) 
{ 

	char tokens[][15] = { "", "KW_AND\n", "KW_OR\n", "KW_NOT\n", "KW_EQUAL\n", "KW_LESS\n", "KW_NIL\n", "KW_LIST\n", "KW_APPEND\n", 	"KW_CONCAT\n", "KW_SET\n", "KW_DEFFUN\n", "KW_FOR\n",
						  "KW_IF\n", "KW_EXIT\n", "KW_LOAD\n", "KW_DISP\n", "KW_TRUE\n", "KW_FALSE\n", "OP_PLUS\n", 		"OP_MINUS\n", "OP_DIV\n", "OP_DBLMULT\n", "OP_MULT\n", "OP_OP\n", "OP_CP\n",
						  "OP_OC\n", "OP_CC\n", "OP_COMMA\n", "COMMENT\n", "IDENTIFIER\n", "VALUE\n", "", ""};
	FILE *fp;
	char filename[100];
	char writestr[100];
	char errMsg[200];
	// Read filename

	strcpy(filename, "parsed_cpp.txt");
	// Read string to write

	fp = fopen(filename,"w+");

	int a = 1;


	FILE *fp2; 
	char filename2[50]; 

	if(argv[1]!=NULL)
	{ //If there is a file as a commandline argument
		strcpy(filename2, argv[1]);	
		fp2 = fopen(filename2,"r"); 
		if(fp2==NULL){
		printf("\nThis file does not exist!\n");
		return -1;
		}
		yyin = fp2; 	

	}

	while(a!=0){ //Read eval print loop or a reading from file loop
		a = yylex();
		
		if(a==END && argv[1]==NULL)	
			break;

		if(a==ERROR || a==UNKNOWN)
		{
			sprintf(errMsg,"Error: \"%s\" cannot be tokenized!\n",errorText);
			writeIntoFile(fp, errMsg);
		}	
		else	
			writeIntoFile(fp, tokens[a]);

	}
	

	fclose(fp); //Close the file

	return 0; 
} 
