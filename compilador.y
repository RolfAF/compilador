%{
extern void yyerror();
extern int yylex();
extern int yylineno;
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>
struct Valor{ int valor_int; float valor_float; char* valor_string; };
struct Valor init_valor();
#define NUMERO_LINHAS 100
struct Simbolo{ char* nome; char* tipo; struct Valor valor; int escopo; int linhas[NUMERO_LINHAS]; };
struct Codigo{  char* cmd; int inicio; int fim; };
#define TAMANHO_TABELA 100
struct Simbolo tabela_simbolos[TAMANHO_TABELA];
struct Codigo  tabela_codigo[TAMANHO_TABELA];
void init_tabela_codigo();
void print_tabela_codigo();
int escopo;
void init_tabela_simbolos();
void print_tabela();
void inicio_codigo();
void fim_codigo();
void fim_codigo();
void declara_variavel();
void declara_variavel_expa();
void att_variavel();
void att_variavel_expa();
void att_variavel_variavel();
void incrementa_variavel();
void decrementa_variavel();
void add_linha();
bool tipo_variavel();
void print_variavel();
void print_valor_variavel();
int existe_variavel();
int get_index();
int valor_int();
float valor_float();
char* valor_string();
bool compara_variaveis();
bool compara_valores_variaveis();
extern char* verde;
extern char* reset;
extern char* azul;
extern char* laranja;
extern char* roxo;
extern char* vermelho;
%} 

%define parse.error verbose

%union {
    char* id;           //nome da variavel
    int   valorint;
    float valorfloat;
    char* valorchar;
    char* tipovar; 
    _Bool logic;
}

%token ABRE_COLCHETES FECHA_COLCHETES QUEBRA_LINHA SAIR TABELA
%token ASPAS_SIMPLES ASPAS_DUPLAS PONTO_VIRGULA VIRGULA 
%token IF ELSE FOR WHILE SWITCH OUT
%token <id> ID
%token <id> INT <valorint> INTVAL <id> FLOAT <valorfloat> FLOATVAL <id> STRING <valorchar> STRINGVAL
%token <id> IGUAL IGUALIGUAL DIFERENTE MAIOR MAIORIGUAL MENOR MENORIGUAL E_LOGICO OU_LOGICO NAO_LOGICO
%token SOMA SUBTRACAO MULTIPLICACAO DIVISAO MAISMAIS MENOSMENOS
%type <id> tipo <logic> expl <valorfloat> expa termo

%%

programa: { init_tabela_simbolos();
            init_tabela_codigo();
            escopo = 0;
            inicio_codigo("programa",yylineno); }
            cmd 
          { printf("%sprograma ✓%s\n",verde, reset); 
            fim_codigo("programa",yylineno); print_tabela(); print_tabela_codigo(); };
    
cmd:  cmd { inicio_codigo("if",yylineno);     } if            { printf("%scmd if ✓     %s\n",verde, reset); fim_codigo("if",yylineno);    }
    | cmd { inicio_codigo("for",yylineno);    } for           { printf("%scmd for ✓    %s\n",verde, reset); fim_codigo("for",yylineno);   }
    | cmd { inicio_codigo("while",yylineno);  } while         { printf("%scmd while ✓  %s\n",verde, reset); fim_codigo("while",yylineno); }
    | cmd funcao                                              { printf("%scmd funcao ✓ %s\n",verde, reset); }
    | cmd out                                                 { printf("%scmd out ✓    %s\n",verde, reset); }
    | cmd att                                                 { printf("%satribuicao ✓ %s\n",verde, reset); }
    | cmd decl                                                { printf("%sdeclaracao ✓ %s\n",verde, reset); }
    | cmd QUEBRA_LINHA
    | cmd SAIR                                                { exit(EXIT_SUCCESS); }
    | cmd TABELA                                              { print_tabela(); print_tabela_codigo(); }
    | %empty;
    
if: IF ABRE_COLCHETES {escopo++;} expl FECHA_COLCHETES linha_opt ABRE_COLCHETES cmd FECHA_COLCHETES else {escopo--;} ;
    
else: ELSE linha_opt { inicio_codigo("else if",yylineno); } if { fim_codigo("else if",yylineno); }
    | ELSE linha_opt { inicio_codigo("else",yylineno); } ABRE_COLCHETES cmd FECHA_COLCHETES { fim_codigo("else",yylineno); } 
    | %empty;
    
for: FOR ABRE_COLCHETES {escopo++;} att PONTO_VIRGULA expl PONTO_VIRGULA att FECHA_COLCHETES ABRE_COLCHETES cmd FECHA_COLCHETES {escopo--;} ;
                                                                                                                                           
while: WHILE ABRE_COLCHETES { escopo++;} expl FECHA_COLCHETES ABRE_COLCHETES cmd FECHA_COLCHETES {escopo--;} ;

funcao: tipo ID ABRE_COLCHETES { inicio_codigo("funcao",yylineno); escopo++;} declaracao_parametros FECHA_COLCHETES linha_opt ABRE_COLCHETES cmd FECHA_COLCHETES { fim_codigo("funcao",yylineno); escopo--;} ;

out:  OUT ABRE_COLCHETES STRINGVAL FECHA_COLCHETES       { printf("%s\n",$3); }
    | OUT ABRE_COLCHETES ID        FECHA_COLCHETES       { if(existe_variavel($3,1)){ print_valor_variavel($3); } }
    | OUT expa                                           { printf("%f\n",$2); };

declaracao_parametros: parametros
    | %empty;
    
parametros: parametros ',' decl
    | decl              { printf("%sdeclaracao ✓ %s\n",verde, reset); };
    
expl: INTVAL    IGUALIGUAL INTVAL    { if( $1 == $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i == %i %s\n",azul,$1,$3,reset); }
    | INTVAL    DIFERENTE  INTVAL    { if( $1 != $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i != %i %s\n",azul,$1,$3,reset); }
    | INTVAL    MAIOR      INTVAL    { if( $1 >  $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i >  %i %s\n",azul,$1,$3,reset); }
    | INTVAL    MAIORIGUAL INTVAL    { if( $1 >= $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i >= %i %s\n",azul,$1,$3,reset); }
    | INTVAL    MENOR      INTVAL    { if( $1 <  $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i <  %i %s\n",azul,$1,$3,reset); }
    | INTVAL    MENORIGUAL INTVAL    { if( $1 <= $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i <= %i %s\n",azul,$1,$3,reset); }
    | ID        IGUALIGUAL INTVAL    { if( valor_int($1) == $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s == %i %s\n",azul,$1,$3,reset); }
    | ID        DIFERENTE  INTVAL    { if( valor_int($1) != $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s != %i %s\n",azul,$1,$3,reset); }
    | ID        MAIOR      INTVAL    { if( valor_int($1) >  $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >  %i %s\n",azul,$1,$3,reset); }
    | ID        MAIORIGUAL INTVAL    { if( valor_int($1) >= $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >= %i %s\n",azul,$1,$3,reset); }
    | ID        MENOR      INTVAL    { if( valor_int($1) <  $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <  %i %s\n",azul,$1,$3,reset); }
    | ID        MENORIGUAL INTVAL    { if( valor_int($1) <= $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <= %i %s\n",azul,$1,$3,reset); }
    | INTVAL    IGUALIGUAL ID        { if( $1 == valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i == %s %s\n",azul,$1,$3,reset); }
    | INTVAL    DIFERENTE  ID        { if( $1 != valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i != %s %s\n",azul,$1,$3,reset); }
    | INTVAL    MAIOR      ID        { if( $1 >  valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i >  %s %s\n",azul,$1,$3,reset); }
    | INTVAL    MAIORIGUAL ID        { if( $1 >= valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i >= %s %s\n",azul,$1,$3,reset); }
    | INTVAL    MENOR      ID        { if( $1 <  valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i <  %s %s\n",azul,$1,$3,reset); }
    | INTVAL    MENORIGUAL ID        { if( $1 <= valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i <= %s %s\n",azul,$1,$3,reset); }
    | FLOATVAL  IGUALIGUAL FLOATVAL  { if( $1 == $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f == %f %s\n",azul,$1,$3,reset); }
    | FLOATVAL  DIFERENTE  FLOATVAL  { if( $1 != $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f != %f %s\n",azul,$1,$3,reset); }
    | FLOATVAL  MAIOR      FLOATVAL  { if( $1 >  $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f >  %f %s\n",azul,$1,$3,reset); }
    | FLOATVAL  MAIORIGUAL FLOATVAL  { if( $1 >= $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f >= %f %s\n",azul,$1,$3,reset); }
    | FLOATVAL  MENOR      FLOATVAL  { if( $1 <  $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f <  %f %s\n",azul,$1,$3,reset); }
    | FLOATVAL  MENORIGUAL FLOATVAL  { if( $1 <= $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f <= %f %s\n",azul,$1,$3,reset); }
    | ID        IGUALIGUAL FLOATVAL  { if( valor_float($1) == $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s == %f %s\n",azul,$1,$3,reset); }
    | ID        DIFERENTE  FLOATVAL  { if( valor_float($1) != $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s != %f %s\n",azul,$1,$3,reset); }
    | ID        MAIOR      FLOATVAL  { if( valor_float($1) >  $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >  %f %s\n",azul,$1,$3,reset); }
    | ID        MAIORIGUAL FLOATVAL  { if( valor_float($1) >= $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >= %f %s\n",azul,$1,$3,reset); }
    | ID        MENOR      FLOATVAL  { if( valor_float($1) <  $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <  %f %s\n",azul,$1,$3,reset); }
    | ID        MENORIGUAL FLOATVAL  { if( valor_float($1) <= $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <= %f %s\n",azul,$1,$3,reset); }
    | FLOATVAL  IGUALIGUAL ID        { if( $1 == valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f == %s %s\n",azul,$1,$3,reset); }
    | FLOATVAL  DIFERENTE  ID        { if( $1 != valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f != %s %s\n",azul,$1,$3,reset); }
    | FLOATVAL  MAIOR      ID        { if( $1 >  valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f >  %s %s\n",azul,$1,$3,reset); }
    | FLOATVAL  MAIORIGUAL ID        { if( $1 >= valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f >= %s %s\n",azul,$1,$3,reset); }
    | FLOATVAL  MENOR      ID        { if( $1 <  valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f <  %s %s\n",azul,$1,$3,reset); }
    | FLOATVAL  MENORIGUAL ID        { if( $1 <= valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f <= %s %s\n",azul,$1,$3,reset); }
    | ID        IGUALIGUAL ID        { $$ = compara_variaveis($1,$3,"igualigual");       add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s == %s %s\n",azul,$1,$3,reset); }
    | ID        DIFERENTE  ID        { $$ = compara_variaveis($1,$3,"diferente");        add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s != %s %s\n",azul,$1,$3,reset); }
    | ID        MAIOR      ID        { $$ = compara_variaveis($1,$3,"maior");            add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s >  %s %s\n",azul,$1,$3,reset); }
    | ID        MAIORIGUAL ID        { $$ = compara_variaveis($1,$3,"maiorigual");       add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s >= %s %s\n",azul,$1,$3,reset); }
    | ID        MENOR      ID        { $$ = compara_variaveis($1,$3,"menor");            add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s <  %s %s\n",azul,$1,$3,reset); }
    | ID        MENORIGUAL ID        { $$ = compara_variaveis($1,$3,"menorigual");       add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s <= %s %s\n",azul,$1,$3,reset); }
    | ID        IGUALIGUAL STRINGVAL { if( !strcmp( valor_string($1) , $3 ) )           { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s == %s %s\n",azul,$1,$3,reset); }
    | ID        DIFERENTE  STRINGVAL { if(  strcmp( valor_string($1) , $3 ) )           { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s != %s %s\n",azul,$1,$3,reset); }
    | ID        MAIOR      STRINGVAL { if(  strcmp( valor_string($1) , $3 ) >  0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >  %s %s\n",azul,$1,$3,reset); }
    | ID        MAIORIGUAL STRINGVAL { if(  strcmp( valor_string($1) , $3 ) >= 0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >= %s %s\n",azul,$1,$3,reset); }
    | ID        MENOR      STRINGVAL { if(  strcmp( valor_string($1) , $3 ) <  0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <  %s %s\n",azul,$1,$3,reset); }
    | ID        MENORIGUAL STRINGVAL { if(  strcmp( valor_string($1) , $3 ) <= 0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <= %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL IGUALIGUAL ID        { if( !strcmp( $1 , valor_string($3) ) )           { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s == %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL DIFERENTE  ID        { if(  strcmp( $1 , valor_string($3) ) )           { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s != %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL MAIOR      ID        { if(  strcmp( $1 , valor_string($3) ) >  0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s >  %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL MAIORIGUAL ID        { if(  strcmp( $1 , valor_string($3) ) >= 0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s >= %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL MENOR      ID        { if(  strcmp( $1 , valor_string($3) ) <  0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s <  %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL MENORIGUAL ID        { if(  strcmp( $1 , valor_string($3) ) <= 0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s <= %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL IGUALIGUAL STRINGVAL { if( !strcmp( $1 , $3 ) )                         { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s == %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL DIFERENTE  STRINGVAL { if(  strcmp( $1 , $3 ) )                         { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s != %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL MAIOR      STRINGVAL { if(  strcmp( $1 , $3 ) >  0 )                    { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s >  %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL MAIORIGUAL STRINGVAL { if(  strcmp( $1 , $3 ) >= 0 )                    { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s >= %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL MENOR      STRINGVAL { if(  strcmp( $1 , $3 ) <  0 )                    { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s <  %s %s\n",azul,$1,$3,reset); }
    | STRINGVAL MENORIGUAL STRINGVAL { if(  strcmp( $1 , $3 ) <= 0 )                    { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s <= %s %s\n",azul,$1,$3,reset); }
    | ABRE_COLCHETES expl FECHA_COLCHETES E_LOGICO  ABRE_COLCHETES expl FECHA_COLCHETES { $$ = ($2 && $4);                                                     printf("%sexpresao logica: e logico (&&)  %s\n",azul,reset); }
    | ABRE_COLCHETES expl FECHA_COLCHETES OU_LOGICO ABRE_COLCHETES expl FECHA_COLCHETES { $$ = ($2 || $4);                                                     printf("%sexpresao logica: ou logico (||) %s\n",azul,reset); }
    | NAO_LOGICO ID                                                                     { $$ = (!$2);                                                          printf("%sexpresao logica: negacao (!) %s\n",azul,reset);    };
                                                                                                                                
att:  ID IGUAL INTVAL           { struct Valor valor = init_valor(valor); valor.valor_int = $3;    att_variavel($1,"int",valor, yylineno);    }               
    | ID IGUAL FLOATVAL         { struct Valor valor = init_valor(valor); valor.valor_float = $3;  att_variavel($1,"float",valor, yylineno);  }               
    | ID IGUAL STRINGVAL        { struct Valor valor = init_valor(valor); valor.valor_string = $3; att_variavel($1,"string",valor, yylineno); }
    | ID IGUAL ID               { att_variavel_variavel($1,$3, yylineno); }
    | ID IGUAL expa             { att_variavel_expa($1,$3, yylineno); }
    | ID MAISMAIS               { incrementa_variavel($1, yylineno); }
    | ID MENOSMENOS             { decrementa_variavel($1, yylineno); }; 
    
expa: ABRE_COLCHETES termo SOMA          termo FECHA_COLCHETES        { $$ = $2 + $4; }              
    | ABRE_COLCHETES termo SUBTRACAO     termo FECHA_COLCHETES        { $$ = $2 - $4; }    
    | ABRE_COLCHETES termo MULTIPLICACAO termo FECHA_COLCHETES        { $$ = $2 * $4; }    
    | ABRE_COLCHETES termo DIVISAO       termo FECHA_COLCHETES        { $$ = $2 / $4; }; // precedencia de operadores desnecessaria devido ao uso obrigatorio de colchetes; 
    
termo: expa
    | ID            { if(!tipo_variavel($1,"string")){ $$ = (float)(tabela_simbolos[get_index($1)].valor.valor_int + tabela_simbolos[get_index($1)].valor.valor_float); } add_linha(get_index($1),yylineno); }
    | INTVAL        { $$ = (float)$1; }
    | FLOATVAL      { $$ = $1;        } ;
    
decl: tipo ID PONTO_VIRGULA     { printf("%sdeclaracao simples: %s %s %s\n",azul,$1,$2,reset); struct Valor valor = init_valor(valor);                          declara_variavel($2, $1, $1,       valor, escopo, yylineno); }
    | tipo ID IGUAL INTVAL      { printf("%sdeclaracao: %s %s = %i %s\n",azul,$1,$2,$4,reset); struct Valor valor = init_valor(valor); valor.valor_int = $4;    declara_variavel($2, $1, "int",    valor, escopo, yylineno);   }
    | tipo ID IGUAL FLOATVAL    { printf("%sdeclaracao: %s %s = %f %s\n",azul,$1,$2,$4,reset); struct Valor valor = init_valor(valor); valor.valor_float = $4;  declara_variavel($2, $1, "float",  valor, escopo, yylineno);   }
    | tipo ID IGUAL STRINGVAL   { printf("%sdeclaracao: %s %s = %s %s\n",azul,$1,$2,$4,reset); struct Valor valor = init_valor(valor); valor.valor_string = $4; declara_variavel($2, $1, "string", valor, escopo, yylineno);   }
    | tipo ID IGUAL ID          { printf("%sdeclaracao: %s %s = %s %s\n",azul,$1,$2,$4,reset); char* tipo_valor = tabela_simbolos[get_index($4)].tipo; declara_variavel($2, $1, tipo_valor, tabela_simbolos[get_index($4)].valor, escopo, yylineno); }
    | tipo ID IGUAL expa        { printf("%sdeclaracao: %s %s = %f %s\n",azul,$1,$2,$4,reset); declara_variavel_expa($2, $1, $4, escopo, yylineno); };

tipo: INT       {$$ = $1;}
    | FLOAT     {$$ = $1;}
    | STRING    {$$ = $1;};

linha_opt: QUEBRA_LINHA
    | %empty;

%%

void add_linha(int index, int linha){
    for(int i = 0; i < 100; i++){
        if(tabela_simbolos[index].linhas[i] == linha){ break; } // linha ja existe no array
        if(tabela_simbolos[index].linhas[i] == 0){              // espaco vazio
            tabela_simbolos[index].linhas[i] = linha;
            break;
        }
    }
}

bool compara_variaveis(char* nome1, char* nome2, char* opl){ //verifica se uma operacao logica entre duas variaves retorna verdadeiro ou falso
    if(existe_variavel(nome1,1) && existe_variavel(nome2,1)){
        int i = get_index(nome1);
        int j = get_index(nome2);
        if(!strcmp(tabela_simbolos[i].tipo,tabela_simbolos[j].tipo)){ // verifica se variaveis sao do mesmo tipo
            char* tipo = tabela_simbolos[i].tipo;
            if(!strcmp(tipo,"int")){
                if(!strcmp(opl,"igualigual")){ return (tabela_simbolos[i].valor.valor_int == tabela_simbolos[j].valor.valor_int); }
                if(!strcmp(opl,"diferente")) { return (tabela_simbolos[i].valor.valor_int != tabela_simbolos[j].valor.valor_int); }
                if(!strcmp(opl,"maior"))     { return (tabela_simbolos[i].valor.valor_int >  tabela_simbolos[j].valor.valor_int); }
                if(!strcmp(opl,"maiorigual")){ return (tabela_simbolos[i].valor.valor_int >= tabela_simbolos[j].valor.valor_int); }
                if(!strcmp(opl,"menor"))     { return (tabela_simbolos[i].valor.valor_int <  tabela_simbolos[j].valor.valor_int); }
                if(!strcmp(opl,"menorigual")){ return (tabela_simbolos[i].valor.valor_int <= tabela_simbolos[j].valor.valor_int); }
            }
            if(!strcmp(tipo,"float")){
                if(!strcmp(opl,"igualigual")){ return (tabela_simbolos[i].valor.valor_float == tabela_simbolos[j].valor.valor_float); }
                if(!strcmp(opl,"diferente")) { return (tabela_simbolos[i].valor.valor_float != tabela_simbolos[j].valor.valor_float); }
                if(!strcmp(opl,"maior"))     { return (tabela_simbolos[i].valor.valor_float >  tabela_simbolos[j].valor.valor_float); }
                if(!strcmp(opl,"maiorigual")){ return (tabela_simbolos[i].valor.valor_float >= tabela_simbolos[j].valor.valor_float); }
                if(!strcmp(opl,"menor"))     { return (tabela_simbolos[i].valor.valor_float <  tabela_simbolos[j].valor.valor_float); }
                if(!strcmp(opl,"menorigual")){ return (tabela_simbolos[i].valor.valor_float <= tabela_simbolos[j].valor.valor_float); }
            }
            if(!strcmp(tipo,"string")){
                int comp = strcmp(tabela_simbolos[i].valor.valor_string, tabela_simbolos[j].valor.valor_string);
                if(!strcmp(opl,"igualigual")){ if(comp == 0){ return true; } else { return false; } }
                if(!strcmp(opl,"diferente")) { if(comp != 0){ return true; } else { return false; } }
                if(!strcmp(opl,"maior"))     { if(comp >  0){ return true; } else { return false; } }
                if(!strcmp(opl,"maiorigual")){ if(comp >= 0){ return true; } else { return false; } }
                if(!strcmp(opl,"menor"))     { if(comp <  0){ return true; } else { return false; } }
                if(!strcmp(opl,"menorigual")){ if(comp <= 0){ return true; } else { return false; } }
            }
        }else{
            printf("%sErro, comparacao de tipos incompativeis para variaveis %s e %s %s\n",vermelho,tabela_simbolos[i].nome,tabela_simbolos[j].nome,reset);
            yyerror();
        }
    }
    return false;
}

int valor_int(char* nome){ //retorna valor int da variavel
    if(existe_variavel(nome,1)){
        int i = get_index(nome);
        if(tipo_variavel(nome,"int")){
            return tabela_simbolos[i].valor.valor_int;
        }else{
            printf("%sErro, comparacao de tipos incompativeis para variavel %s: %s != int %s\n",vermelho,nome,tabela_simbolos[i].tipo,reset);
            yyerror();
        }
    }
    return 0;
}

float valor_float(char* nome){ //retorna valor float da variavel
    if(existe_variavel(nome,1)){
        int i = get_index(nome);
        if(tipo_variavel(nome,"float")){
            return tabela_simbolos[i].valor.valor_float;
        }else{
            printf("%sErro, comparacao de tipos incompativeis para variavel %s: %s != float %s\n",vermelho,nome,tabela_simbolos[i].tipo,reset);
            yyerror();
        }
    }
    return 0;
}

char* valor_string(char* nome){ //retorna valor string da variavel
    if(existe_variavel(nome,1)){
        int i = get_index(nome);
        if(tipo_variavel(nome,"string")){
            return tabela_simbolos[i].valor.valor_string;
        }else{
            printf("%sErro, comparacao de tipos incompativeis para variavel %s: %s != string %s\n",vermelho,nome,tabela_simbolos[i].tipo,reset);
            yyerror();
        }
    }
    return "";
}

struct Valor init_valor(struct Valor valor){
    valor.valor_int = 0;
    valor.valor_float = 0;
    valor.valor_string = "";
    return valor;
}

int existe_variavel(char* nome, int erro){
    if(get_index(nome) != -1){
        return 1;
    }else if(erro){
        printf("%sErro, variavel %s nao declarada%s\n",vermelho,nome,reset);
        yyerror();
    }
    return 0;
}

int nao_existe_variavel(char* nome, int erro){
    if(get_index(nome) == -1){
        return 1;
    }else if(erro){
        printf("%sErro, variavel %s ja declarada%s\n",vermelho,nome,reset);
        yyerror();
    }
    return 0;
}

int get_index(char* nome){
    for(int i = 0; i < TAMANHO_TABELA; i++){
        if(!strcmp(tabela_simbolos[i].nome, nome)){
            return i; // variavel encontrada neste indice
        }
    }
    return -1; //variavel nao encontrada na tabela
}

void declara_variavel(char* nome, char* tipo, char* tipo_valor, struct Valor val, int escopo, int linha){ //busca nome da variavel na lista, se nao existir, adiciona
    if(nao_existe_variavel(nome,1)){
        if(!strcmp(tipo,tipo_valor)){ //tipo da variavel e tipo do valor sao iguais
            for(int i = 0; i < TAMANHO_TABELA; i++){
                if(!strcmp(tabela_simbolos[i].nome, "")){
                    tabela_simbolos[i].nome = nome;
                    tabela_simbolos[i].tipo = tipo;
                    tabela_simbolos[i].valor = val;
                    tabela_simbolos[i].escopo = escopo;
                    add_linha(i,linha);
                    printf("%s",roxo);
                    printf("add %s \n", nome);
                    printf("%s",reset);
                    break; 
                }        
            }
        }else{
            printf("%sErro, atribuicao de tipos incompativeis para variavel %s: %s != %s %s\n",vermelho,nome,tipo,tipo_valor,reset);
            yyerror();
        }
    }
}

void declara_variavel_expa(char* nome, char* tipo, double valor, int escopo, int linha){
    if(nao_existe_variavel(nome,1)){
        if(strcmp(tipo,"string")){ // tipo nao e string
            for(int i = 0; i < TAMANHO_TABELA; i++){
                if(!strcmp(tabela_simbolos[i].nome, "")){
                    tabela_simbolos[i].nome = nome;
                    tabela_simbolos[i].tipo = tipo;
                    if( !strcmp(tipo,"int") )  { tabela_simbolos[i].valor.valor_int   = (int)valor; }
                    if( !strcmp(tipo,"float") ){ tabela_simbolos[i].valor.valor_float = valor; }
                    tabela_simbolos[i].escopo = escopo;
                    add_linha(i,linha);
                    printf("%s",roxo);
                    printf("add %s \n", nome);
                    printf("%s",reset);
                    break; 
                }       
            }
        }else{
            printf("%sErro, atribuicao de tipos incompativeis para variavel %s: string != int/float %s\n",vermelho,nome,reset);
            yyerror();
        }
    }
}

void att_variavel(char* nome, char* tipo, struct Valor val, int linha){
    if(existe_variavel(nome,1)){
        int i = get_index(nome);
        if(!strcmp(tabela_simbolos[i].tipo, tipo)){ 
            tabela_simbolos[i].valor = val;
            add_linha(i,linha);
            printf("%satt %s%s\n",roxo,nome,reset);
        }else{
            printf("%sErro, atribuicao de tipos incompativeis para variavel %s: %s != %s %s\n",vermelho,nome,tabela_simbolos[i].tipo,tipo,reset);
            yyerror();
        }
    }
}

void att_variavel_expa(char* nome, double valor, int linha){
    if(existe_variavel(nome,1)){
        int i = get_index(nome);
        if(!tipo_variavel(nome,"string")){
            if(tipo_variavel(nome,"int"))  { tabela_simbolos[i].valor.valor_int   = (int)valor; }
            if(tipo_variavel(nome,"float")){ tabela_simbolos[i].valor.valor_float = valor; }
            add_linha(i,linha);
            printf("%satt %s%s\n",roxo,nome,reset);
        }else{
            printf("%sErro, atribuicao de tipos incompativeis para variavel %s: string != int/float %s\n",vermelho,nome,reset);
            yyerror();
        }
    }
}

void att_variavel_variavel(char* nome1, char* nome2, int linha){
    if(existe_variavel(nome1,1) && existe_variavel(nome2,1)){
        int i = get_index(nome1);
        int j = get_index(nome2);
        if(!strcmp(tabela_simbolos[i].tipo,tabela_simbolos[j].tipo)){ // verifica se variaveis sao do mesmo tipo
            tabela_simbolos[i].valor = tabela_simbolos[j].valor;
            add_linha(i,linha);
        }else{
            printf("%sErro, atribuicao entre variaveis de tipos incompativeis: %s e %s %s\n",vermelho,nome1,nome2,reset);
            yyerror();
        }
    }
}

void incrementa_variavel(char* nome, int linha){
    if(existe_variavel(nome,1)){
        int i = get_index(nome);
        if(tipo_variavel(nome,"int")){   tabela_simbolos[i].valor.valor_int += 1;   add_linha(i,linha); }
        if(tipo_variavel(nome,"float")){ tabela_simbolos[i].valor.valor_float += 1; add_linha(i,linha); }
        if(tipo_variavel(nome,"string")){ 
            printf("%sErro, atribuicao de tipos incompativeis para variavel %s: string != int/float %s\n",vermelho,nome,reset);
            yyerror(); 
        }        
    }
}

void decrementa_variavel(char* nome, int linha){
    if(existe_variavel(nome,1)){
        int i = get_index(nome);
        if(tipo_variavel(nome,"int")){   tabela_simbolos[i].valor.valor_int -= 1;   add_linha(i,linha); }
        if(tipo_variavel(nome,"float")){ tabela_simbolos[i].valor.valor_float -= 1; add_linha(i,linha); }
        if(tipo_variavel(nome,"string")){ 
            printf("%sErro, atribuicao de tipos incompativeis para variavel %s: string != int/float %s\n",vermelho,nome,reset);
            yyerror(); 
        }        
    }
}

bool tipo_variavel(char* nome, char* tipo){
    if(existe_variavel(nome,0)){
        int i = get_index(nome);
        if(!strcmp(tabela_simbolos[i].tipo,tipo)){ return true; }
    }
    return false;
}

void print_variavel(char* nome){
    int i = get_index(nome);
    printf("%svariavel:%s %s %stipo:%s %s ",roxo,reset,tabela_simbolos[i].nome,roxo,reset, tabela_simbolos[i].tipo); 
    if(!strcmp(tabela_simbolos[i].tipo, "int"))   { printf("%svalor:%s %i ",roxo,reset,tabela_simbolos[i].valor.valor_int);    }            
    if(!strcmp(tabela_simbolos[i].tipo, "float")) { printf("%svalor:%s %f ",roxo,reset,tabela_simbolos[i].valor.valor_float);  }            
    if(!strcmp(tabela_simbolos[i].tipo, "string")){ printf("%svalor:%s %s ",roxo,reset,tabela_simbolos[i].valor.valor_string); }
    printf("%sescopo:%s %i ",roxo,reset,tabela_simbolos[i].escopo);
    printf("%slinhas:%s",roxo,reset);
    int j = 0;
    char* virgula;
    while(tabela_simbolos[i].linhas[j] != 0){
        printf("%s %i",virgula,tabela_simbolos[i].linhas[j]);
        virgula = ",";
        j++;
    }
    virgula = "";
    printf("\n");
}

void print_valor_variavel(char* nome){
    int i = get_index(nome);
    if(!strcmp(tabela_simbolos[i].tipo, "int"))   { printf("%i\n",tabela_simbolos[i].valor.valor_int);    }            
    if(!strcmp(tabela_simbolos[i].tipo, "float")) { printf("%f\n",tabela_simbolos[i].valor.valor_float);  }            
    if(!strcmp(tabela_simbolos[i].tipo, "string")){ printf("%s\n",tabela_simbolos[i].valor.valor_string); }
}

void print_tabela(){
    printf("%s",roxo);
    printf("tabela de simbolos\n");
    for(int i = 0; i < TAMANHO_TABELA; i++){
        if(strcmp(tabela_simbolos[i].nome, "")){
            print_variavel(tabela_simbolos[i].nome);
        }
    }
    printf("%s",reset);
}

void init_tabela_simbolos(){
    for(int i = 0; i < TAMANHO_TABELA; i++){
        tabela_simbolos[i].nome = "";
        tabela_simbolos[i].valor.valor_int = 0;
        tabela_simbolos[i].valor.valor_float = 0;
        tabela_simbolos[i].valor.valor_string = "";
    }
}

void init_tabela_codigo(){
    for(int i = 0; i < TAMANHO_TABELA; i++){
        tabela_codigo[i].cmd = "";
        tabela_codigo[i].inicio = 0;
        tabela_codigo[i].fim = 0;
    }
}

void inicio_codigo(char* cmd, int linha){
    for(int i = 0; i < TAMANHO_TABELA; i++){
        if(tabela_codigo[i].cmd == ""){ //adiciona o comando no primeiro espaco vaziu
            tabela_codigo[i].cmd = cmd;
            tabela_codigo[i].inicio = linha;
            break;
        }
    }
}

void fim_codigo(char* cmd, int linha){
    for(int i = TAMANHO_TABELA-1; i >= 0; i--){
        if(tabela_codigo[i].cmd == cmd && tabela_codigo[i].fim == 0){ //adiciona o fim ao comando mais abaixo (pilha)
            tabela_codigo[i].fim = linha;
            break;
        }
    }    
}

void print_tabela_codigo(){
    printf("%s",laranja);
    printf("tabela de codigo\n");
    for(int i = 0; i < TAMANHO_TABELA; i++){
        if(strcmp(tabela_codigo[i].cmd, "")){
            printf("%scomando:%s %s %sinicio:%s %i %sfim:%s %i \n",laranja,reset,tabela_codigo[i].cmd,laranja,reset,tabela_codigo[i].inicio,laranja,reset,tabela_codigo[i].fim); 
        }
    }
    printf("%s",reset);
}

int main(int argc, char **argv){
    printf("----------------------------------------------------------------------------------\n");
    printf("Iniciando...\n\n");
    return yyparse ( );    
}

void yyerror (char *s) {
    printf("Linha: %i\n%s\n",yylineno,s);
    fprintf (stderr, "%s",s); } 
