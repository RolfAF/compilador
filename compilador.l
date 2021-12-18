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
char* amarelo="\e[38;2;255;225;0m";
char* verde="\e[38;2;53;255;145m";
char* azul="\e[38;2;100;200;255m";
char* laranja="\e[38;2;255;150;50m";
char* roxo="\e[38;2;200;125;255m";
char* vermelho="\e[38;2;255;0;0m";
char* reset="\e[0m";

%} 

%% 

"int"                                   cond_print("tipo_int","");              {yylval.id = strdup(yytext);            return INT;}
[0-9]+                                  cond_print("valor_int:",yytext);        {yylval.valorint = atoi(yytext);        return INTVAL;}
"float"                                 cond_print("tipo_float","");            {yylval.id = strdup(yytext);            return FLOAT;}       
[0-9]+"."[0-9]+                         cond_print("valor_float:",yytext);      {yylval.valorfloat = atof(yytext);      return FLOATVAL;}
"string"                                cond_print("tipo_string","");           {yylval.id = strdup(yytext);            return STRING;}
\'([^\\\']|\\.)*\'                      cond_print("valor_string:",yytext);     {yylval.valorchar = strdup(yytext);     return STRINGVAL;}
"if"                                    cond_print("'if'","");                                                          return IF;  
"else"                                  cond_print("'else'","");                                                        return ELSE;  
"for"                                   cond_print("'for'","");                                                         return FOR; 
"while"                                 cond_print("'while'","");                                                       return WHILE;   
"out"                                   cond_print("'out'","");                                                         return OUT;   
"//".*                                  cond_print("comentario","");
"&&"|"and"                              cond_print("'&&'","");                                                          return E_LOGICO;
"||"|"or"                               cond_print("'||'","");                                                          return OU_LOGICO;
"!"|"not"                               cond_print("'!'","");                                                           return NAO_LOGICO;
"=="|"-eq"                              cond_print("'=='","");                                                          return IGUALIGUAL;
"!=""-df"                               cond_print("'!='","");                                                          return DIFERENTE;
">"|"-gt"                               cond_print("'>'","");                                                           return MAIOR;
">="|"-ge"                              cond_print("'>='","");                                                          return MAIORIGUAL;
"<"|"-lt"                               cond_print("'<'","");                                                           return MENOR;
"<="|"-le"                              cond_print("'<='","");                                                          return MENORIGUAL;
"exit"|"sair"                                                                                                           return SAIR;
"tabela"                                                                                                                return TABELA;
[a-zA-Z]([_a-zA-Z0-9]*[a-zA-Z0-9])?     cond_print("identificador:",yytext);    {yylval.id = strdup(yytext);            return ID;}
"="                                     cond_print("'='","");                                                           return IGUAL;
"+"                                     cond_print("'+'","");                                                           return SOMA;
"-"                                     cond_print("'-'","");                                                           return SUBTRACAO;
"*"                                     cond_print("'*'","");                                                           return MULTIPLICACAO;
"/"                                     cond_print("'/'","");                                                           return DIVISAO;
"++"                                    cond_print("'++'","");                                                          return MAISMAIS;
"--"                                    cond_print("'--'","");                                                          return MENOSMENOS;
"["                                     cond_print("'['","");                                                           return ABRE_COLCHETES;
"]"                                     cond_print("']'","");                                                           return FECHA_COLCHETES;
\n                                      espacador[0] = '\0';                    yylineno++;                             return QUEBRA_LINHA;
";"                                     cond_print("';'","");                                                           return PONTO_VIRGULA;
[ \t]                                   set_espacador(yytext);
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
int yywrap(void)
{
    return 1;
}
