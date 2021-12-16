%{
#include "test1.tab.h"

#include <string.h>
//strlen(yytext)

void yyerror (char *s);
int yylex();
char spacer[50];
int verbose = 1;
void set_spacer();
void cond_print();
char* amarelo="\e[38;2;255;225;0m";
char* verde="\e[38;2;53;255;145m";
char* azul="\e[38;2;100;200;255m";
char* roxo="\e[38;2;200;125;255m";
char* vermelho="\e[38;2;255;0;0m";
char* reset="\e[0m";

%} 

%% 

"int"                                   cond_print("tipo_int","");                  {yylval.id = strdup(yytext);            return INT;}
[0-9]+                                  cond_print("valor_int:",yytext);               {yylval.valorint = atoi(yytext);        return INTVAL;}
"float"                                 cond_print("tipo_float","");      {yylval.id = strdup(yytext);            return FLOAT;}       
[0-9]+"."[0-9]+                         cond_print("valor_float:",yytext);      {yylval.valorfloat = atof(yytext);      return FLOATVAL;}
"string"                                cond_print("tipo_string","");      {yylval.id = strdup(yytext);            return STRING;}
\'([^\\\']|\\.)*\'                      cond_print("valor_string:",yytext);      {yylval.valorchar = strdup(yytext);     return STRINGVAL;}
"if"                                    cond_print("'if'","");                                              return IF;  
"else"                                  cond_print("'else'","");                                              return ELSE;  
"for"                                   cond_print("'for'","");                                              return FOR; 
"while"                                 cond_print("'while'","");                                              return WHILE;   
"out"                                   cond_print("'out'","");                                              return OUT;   
"//".*                                  cond_print("comentario","");
[a-zA-Z]([_a-zA-Z0-9]*[a-zA-Z0-9])?     cond_print("identificador:",yytext);      {yylval.id = strdup(yytext);            return ID;}
"="                                     cond_print("'='","");              return IGUAL;
"+"                                     cond_print("'+'","");                                                                   return SOMA;
"-"                                     cond_print("'-'","");                                                                   return SUBTRACAO;
"*"                                     cond_print("'*'","");                                                                   return MULTIPLICACAO;
"/"                                     cond_print("'/'","");                                                                   return DIVISAO;
"=="                                    cond_print("'=='","");                                                                   return IGUALIGUAL;
"!="                                    cond_print("'!='","");                                                                   return DIFERENTE;
">"                                     cond_print("'>'","");                                                                   return MAIOR;
">="                                    cond_print("'>='","");                                                                   return MAIORIGUAL;
"<"                                     cond_print("'<'","");                                                                   return MENOR;
"<="                                    cond_print("'<='","");                                                                   return MENORIGUAL;
"++"                                    cond_print("'++'","");                                                                   return MAISMAIS;
"--"                                    cond_print("'--'","");                                                                   return MENOSMENOS;
"["                                     cond_print("'['","");                                                                   return ABRE_COLCHETES;
"]"                                     cond_print("']'","");                                                                   return FECHA_COLCHETES;
\n                                      /*printf("'endl'\n");*/ spacer[0] = '\0'; yylineno++;                                                     return QUEBRA_LINHA;
";"                                     /*printf("';'\n");*/                                                                    return PONTO_VIRGULA;
[ \t]                                   set_spacer(yytext);
.                                       {printf("Caracter Invalido\n");}

%%

// void caracterInvalido(){
//     printf("Caracter Invalido");
// }
//[a-zA-Z]([_a-zA-Z0-9]*[a-zA-Z0-9])?     printf("identificador: %s\n",yytext);  {yylval.id = yytext;                return ID;}

void cond_print(char* token, char* text){
    if(verbose){
        printf("%s%s└────────────%s %s%s \n", amarelo, spacer, token, reset, text);
    }
    set_spacer(yytext); 
}

void set_spacer(char* text){
    char* aux = " ";
    for(int i = 0; i < strlen(text); i++){
        strcat(spacer,aux);
    }
}
int yywrap(void)
{
    return 1;
}

// void yyerror(char *s) {
//     fprintf(stderr, "\nERROR ON LINE %d : \n %s\n", yylineno, s);
//     exit(0);
// }
// flex test1.l; cc lex.yy.c -ll; ./a.out
