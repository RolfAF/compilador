%{
#include "compilador.tab.h"
#include <string.h>
#include <stdbool.h>

void yyerror (char *s);
int yylex();
char espacador[100];
int print = 1;
void set_espacador();
void cond_print();
char* rem_aspas();
char* amarelo="\e[38;2;255;225;0m";
char* verde="\e[38;2;53;255;145m";
char* azul="\e[38;2;100;200;255m";
char* laranja="\e[38;2;255;150;50m";
char* roxo="\e[38;2;200;125;255m";
char* vermelho="\e[38;2;255;0;0m";
char* reset="\e[0m";
char* nova_string;

%} 

%% 

"int"                                   cond_print("tipo_int","");              {yylval.id = strdup(yytext);                       return INT;              }
[0-9]+                                  cond_print("valor_int:",yytext);        {yylval.valorint = atoi(yytext);                   return INTVAL;           }
"float"                                 cond_print("tipo_float","");            {yylval.id = strdup(yytext);                       return FLOAT;            }       
[0-9]+"."[0-9]+                         cond_print("valor_float:",yytext);      {yylval.valorfloat = atof(yytext);                 return FLOATVAL;         } 
"string"                                cond_print("tipo_string","");           {yylval.id = strdup(yytext);                       return STRING;           }
\'([^\\\']|\\.)*\'                      cond_print("valor_string:",yytext);     {yylval.valorchar = strdup(yytext);                return STRINGVAL;        }
"if"                                    cond_print("'if'","");                                                                     return IF;  
"else"                                  cond_print("'else'","");                                                                   return ELSE;  
"for"                                   cond_print("'for'","");                                                                    return FOR; 
"while"                                 cond_print("'while'","");                                                                  return WHILE;   
"out"                                   cond_print("'out'","");                                                                    return OUT;   
"//".*                                  cond_print("comentario","");                                                               
"&&"|"and"                              cond_print("'&&'","");                  {yylval.id = "&&";                                 return E_LOGICO;         }
"||"|"or"                               cond_print("'||'","");                  {yylval.id = "||";                                 return OU_LOGICO;        }
"!"|"not"                               cond_print("'!'","");                   {yylval.id = "!";                                  return NAO_LOGICO;       }
"=="|"-eq"                              cond_print("'=='","");                  {yylval.id = "==";                                 return IGUALIGUAL;       }
"!=""-df"                               cond_print("'!='","");                  {yylval.id = "!=";                                 return DIFERENTE;        }
">"|"-gt"                               cond_print("'>'","");                   {yylval.id = ">";                                  return MAIOR;            }
">="|"-ge"                              cond_print("'>='","");                  {yylval.id = ">=";                                 return MAIORIGUAL;       }
"<"|"-lt"                               cond_print("'<'","");                   {yylval.id = "<";                                  return MENOR;            }
"<="|"-le"                              cond_print("'<='","");                  {yylval.id = "<=";                                 return MENORIGUAL;       }
"exit"|"sair"                                                                                                                      return SAIR;
"tabela"                                                                                                                           return TABELA;
[a-zA-Z]([_a-zA-Z0-9]*[a-zA-Z0-9])?     cond_print("identificador:",yytext);    {yylval.id = strdup(yytext);                       return ID;               }
"="                                     cond_print("'='","");                   {yylval.id = "=";                                  return IGUAL;            }
"+"                                     cond_print("'+'","");                   {yylval.id = "+";                                  return SOMA;             }
"-"                                     cond_print("'-'","");                   {yylval.id = "-";                                  return SUBTRACAO;        }
"*"                                     cond_print("'*'","");                   {yylval.id = "*";                                  return MULTIPLICACAO;    }
"/"                                     cond_print("'/'","");                   {yylval.id = "/";                                  return DIVISAO;          }
"++"                                    cond_print("'++'","");                  {yylval.id = "++";                                 return MAISMAIS;         }
"--"                                    cond_print("'--'","");                  {yylval.id = "--";                                 return MENOSMENOS;       }
"["                                     cond_print("'['","");                                                                      return ABRE_COLCHETES;
"]"                                     cond_print("']'","");                                                                      return FECHA_COLCHETES;
\n                                      espacador[0] = '\0';                    yylineno++;                                        return QUEBRA_LINHA;
";"                                     cond_print("';'","");                                                                      return PONTO_VIRGULA;
","                                     cond_print("','","");                                                                      return VIRGULA;
[ ]                                     set_espacador(yytext);
[\t]                                    set_espacador(yytext);                                                                     return TAB;
.                                       {printf("Caracter Invalido\n");}

%%

void cond_print(char* token, char* text){
    if(print){
        printf("%s%s└────────────%s %s%s \n", amarelo, espacador, token, reset, text);
    }
    set_espacador(yytext); 
}

void set_espacador(char* text){
    char* aux = " ";
    for(int i = 0; i < strlen(text); i++){
        strcat(espacador,aux);
    }
}

char* rem_aspas(char* string){
    printf("remove aspas\n");
    //char nova_string[strlen(string)-2];
    //char *nova_string = malloc(strlen(string)-2);
    nova_string = "";
    for(int i = 1; i < strlen(string) - 1; i++){
        printf("%c",string[i]);
    }
    printf("%s\n",nova_string);
    //char *pointer = nova_string;
    //printf("%s\n",(char*)pointer);
    return nova_string;
}

int yywrap(void){ return 1; }
