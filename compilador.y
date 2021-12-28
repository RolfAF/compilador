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
void init_arquivo();
void escreve_arquivo();
char* tipo_print();
int existe_variavel();
int get_index();
int valor_int();
float valor_float();
float valor_numerico();
char* valor_string();
bool compara_variaveis();
bool compara_valores_variaveis();
char* string_temp;
int numero_erros = 0;
void compila_codigo();
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

%token ABRE_COLCHETES FECHA_COLCHETES QUEBRA_LINHA SAIR TABELA TAB
%token ASPAS_SIMPLES ASPAS_DUPLAS PONTO_VIRGULA VIRGULA 
%token IF ELSE FOR WHILE SWITCH OUT
%token <id> ID
%token <id> INT <valorint> INTVAL <id> FLOAT <valorfloat> FLOATVAL <id> STRING <valorchar> STRINGVAL
%token <id> IGUAL IGUALIGUAL DIFERENTE MAIOR MAIORIGUAL MENOR MENORIGUAL E_LOGICO OU_LOGICO NAO_LOGICO
%token <id> SOMA SUBTRACAO MULTIPLICACAO DIVISAO MAISMAIS MENOSMENOS
%type  <id> tipo <logic> expl <valorfloat> expa

%%

programa: { init_tabela_simbolos();                  }
          { init_tabela_codigo();                    }
          { escopo = 0;                              }
          { inicio_codigo("programa",yylineno);      }
          { init_arquivo();                          } 
            cmd 
          { printf("%sprograma ✓%s\n",verde, reset); } 
          { fim_codigo("programa",yylineno);         }
          { print_tabela();                          }
          { print_tabela_codigo();                   }
          { compila_codigo();                        } ;
    
cmd:  cmd { inicio_codigo("if",yylineno);     } if              { printf("%sif ✓     %s\n",verde, reset); fim_codigo("if",yylineno);    }
    | cmd { inicio_codigo("for",yylineno);    } for             { printf("%sfor ✓    %s\n",verde, reset); fim_codigo("for",yylineno);   }
    | cmd { inicio_codigo("while",yylineno);  } while           { printf("%swhile ✓  %s\n",verde, reset); fim_codigo("while",yylineno); }
    | cmd funcao                                                { printf("%sfuncao ✓ %s\n",verde, reset);                               }
    | cmd chamada_funcao                                        { printf("%schamada funcao ✓ %s\n",verde, reset);                       }
    | cmd out                                                   { printf("%sout ✓    %s\n",verde, reset);                               }
    | cmd att                                                   { printf("%satribuicao ✓ %s\n",verde, reset);                           }
    | cmd decl                                                  { printf("%sdeclaracao ✓ %s\n",verde, reset);                           }
    | cmd QUEBRA_LINHA                                          { escreve_arquivo("\n");                                                }
    | cmd SAIR                                                  { exit(EXIT_SUCCESS);                                                   }
    | cmd TABELA                                                { print_tabela(); print_tabela_codigo();                                }
    | %empty;
    
if: IF ABRE_COLCHETES                                           { escopo++; escreve_arquivo("if("); } 
    expl FECHA_COLCHETES linha_opt ABRE_COLCHETES               { escreve_arquivo("){");            } 
    cmd FECHA_COLCHETES                                         { escreve_arquivo("}");             }
    else                                                        { escopo--;                         };
                                                                
else: ELSE linha_opt                                            { inicio_codigo("else if",yylineno); escreve_arquivo("else "); } 
      if                                                        { fim_codigo("else if",yylineno);                              }
    | ELSE linha_opt                                            { inicio_codigo("else",yylineno);                              } 
      ABRE_COLCHETES                                            { escreve_arquivo("else{");                                    }
      cmd FECHA_COLCHETES                                       { fim_codigo("else",yylineno); escreve_arquivo("}");           } 
    | %empty;                                                   
                                                                
for: FOR ABRE_COLCHETES                      { escopo++; escreve_arquivo("for("); } 
     decl PONTO_VIRGULA expl PONTO_VIRGULA   { escreve_arquivo(";");              }
     for_incr FECHA_COLCHETES ABRE_COLCHETES { escreve_arquivo("){");             }
     cmd FECHA_COLCHETES                     { escopo--; escreve_arquivo("}");    } ;
                                                                
for_incr: ID MAISMAIS { char linha[100] = ""; snprintf(linha, sizeof(linha),"%s++",$1); escreve_arquivo(linha); } 
    | ID MENOSMENOS   { char linha[100] = ""; snprintf(linha, sizeof(linha),"%s--",$1); escreve_arquivo(linha); } ;
                                                                                                                                                       
while: WHILE ABRE_COLCHETES                                     { escopo++; escreve_arquivo("while("); } 
       expl FECHA_COLCHETES ABRE_COLCHETES                      { escreve_arquivo("){");               }
       cmd FECHA_COLCHETES                                      { escopo--; escreve_arquivo("}");      } ;

funcao: tipo ID ABRE_COLCHETES                                                  { inicio_codigo("funcao",yylineno); escopo++; char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s(",$1,$2); escreve_arquivo(linha); } 
        declaracao_parametros_func FECHA_COLCHETES linha_opt ABRE_COLCHETES     { escreve_arquivo("){");                                                                                                                   }
        cmd FECHA_COLCHETES                                                     { fim_codigo("funcao",yylineno); escopo--; escreve_arquivo("}");                                                                           } ;
        
declaracao_parametros_func: parametros_func
    | %empty;
        
parametros_func: parametros_func VIRGULA tipo ID                { printf("%sdeclaracao parametro ✓ %s\n",verde, reset); struct Valor valor = init_valor(valor); declara_variavel($4, $3, $3, valor, escopo, yylineno); char linha[100] = ""; snprintf(linha, sizeof(linha),", %s %s ",$3,$4); escreve_arquivo(linha); }
    | tipo ID                                                   { printf("%sdeclaracao parametro ✓ %s\n",verde, reset); struct Valor valor = init_valor(valor); declara_variavel($2, $1, $1, valor, escopo, yylineno); char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s",$1,$2);    escreve_arquivo(linha); };
        
chamada_funcao: ID                                              { char linha[100] = ""; snprintf(linha, sizeof(linha),"%s(",$1); escreve_arquivo(linha); } 
                ABRE_COLCHETES chamada_parametros               { escreve_arquivo(");");                                                                 }
                FECHA_COLCHETES ;

chamada_parametros: chamada_parametro
    | %empty;

chamada_parametro: chamada_parametro VIRGULA { escreve_arquivo(", "); } chamada_termo      
    | chamada_termo;                                             

chamada_termo: ID       { char linha[100] = ""; snprintf(linha, sizeof(linha),"%s",$1); escreve_arquivo(linha); }
    | INTVAL            { char linha[100] = ""; snprintf(linha, sizeof(linha),"%i",$1); escreve_arquivo(linha); }
    | FLOATVAL          { char linha[100] = ""; snprintf(linha, sizeof(linha),"%f",$1); escreve_arquivo(linha); }
    | STRINGVAL         { char linha[100] = ""; snprintf(linha, sizeof(linha),"%s",$1); escreve_arquivo(linha); } ;

out:  OUT ABRE_COLCHETES STRINGVAL FECHA_COLCHETES       { printf("%s\n",$3);                                       char linha[100] = ""; snprintf(linha, sizeof(linha),"printf(\"%s\\n\");",$3); escreve_arquivo(linha);                                        }
    | OUT ABRE_COLCHETES ID        FECHA_COLCHETES       { if(existe_variavel($3,1)){ print_valor_variavel($3); }   char linha[100] = ""; snprintf(linha, sizeof(linha),"printf(\"%s\\n\",%s",tipo_print($3),$3); escreve_arquivo(linha); escreve_arquivo(");"); }
    | OUT expa                                           { printf("%f\n",$2);                                       escreve_arquivo("printf(\"%f\\n\",(float)"); escreve_arquivo(string_temp); escreve_arquivo(");");                                            };
                                                                                                                                                                                                                                                            
expl: INTVAL    IGUALIGUAL INTVAL    { if( $1 == $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i == %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %i",$1,$2,$3); escreve_arquivo(linha); }             
    | INTVAL    DIFERENTE  INTVAL    { if( $1 != $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i != %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    MAIOR      INTVAL    { if( $1 >  $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i >  %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    MAIORIGUAL INTVAL    { if( $1 >= $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i >= %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    MENOR      INTVAL    { if( $1 <  $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i <  %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    MENORIGUAL INTVAL    { if( $1 <= $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %i <= %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    IGUALIGUAL ID        { if( $1 == valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i == %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    DIFERENTE  ID        { if( $1 != valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i != %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    MAIOR      ID        { if( $1 >  valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i >  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    MAIORIGUAL ID        { if( $1 >= valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i >= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    MENOR      ID        { if( $1 <  valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i <  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | INTVAL    MENORIGUAL ID        { if( $1 <= valor_int($3) )                        { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %i <= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%i %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  IGUALIGUAL FLOATVAL  { if( $1 == $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f == %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  DIFERENTE  FLOATVAL  { if( $1 != $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f != %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  MAIOR      FLOATVAL  { if( $1 >  $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f >  %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  MAIORIGUAL FLOATVAL  { if( $1 >= $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f >= %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  MENOR      FLOATVAL  { if( $1 <  $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f <  %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  MENORIGUAL FLOATVAL  { if( $1 <= $3 )                                   { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %f <= %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  IGUALIGUAL ID        { if( $1 == valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f == %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  DIFERENTE  ID        { if( $1 != valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f != %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  MAIOR      ID        { if( $1 >  valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f >  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  MAIORIGUAL ID        { if( $1 >= valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f >= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  MENOR      ID        { if( $1 <  valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f <  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | FLOATVAL  MENORIGUAL ID        { if( $1 <= valor_float($3) )                      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %f <= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%f %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | ID        IGUALIGUAL INTVAL    { if( valor_int($1) == $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s == %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | ID        DIFERENTE  INTVAL    { if( valor_int($1) != $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s != %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MAIOR      INTVAL    { if( valor_int($1) >  $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >  %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MAIORIGUAL INTVAL    { if( valor_int($1) >= $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >= %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MENOR      INTVAL    { if( valor_int($1) <  $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <  %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MENORIGUAL INTVAL    { if( valor_int($1) <= $3 )                        { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <= %i ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %i",$1,$2,$3); escreve_arquivo(linha); }
    | ID        IGUALIGUAL FLOATVAL  { if( valor_float($1) == $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s == %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | ID        DIFERENTE  FLOATVAL  { if( valor_float($1) != $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s != %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MAIOR      FLOATVAL  { if( valor_float($1) >  $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >  %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MAIORIGUAL FLOATVAL  { if( valor_float($1) >= $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >= %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MENOR      FLOATVAL  { if( valor_float($1) <  $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <  %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MENORIGUAL FLOATVAL  { if( valor_float($1) <= $3 )                      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <= %f ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %f",$1,$2,$3); escreve_arquivo(linha); }
    | ID        IGUALIGUAL ID        { $$ = compara_variaveis($1,$3,"igualigual");       add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s == %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | ID        DIFERENTE  ID        { $$ = compara_variaveis($1,$3,"diferente");        add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s != %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MAIOR      ID        { $$ = compara_variaveis($1,$3,"maior");            add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s >  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MAIORIGUAL ID        { $$ = compara_variaveis($1,$3,"maiorigual");       add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s >= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MENOR      ID        { $$ = compara_variaveis($1,$3,"menor");            add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s <  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MENORIGUAL ID        { $$ = compara_variaveis($1,$3,"menorigual");       add_linha(get_index($1),yylineno); add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s <= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | ID        IGUALIGUAL STRINGVAL { if( !strcmp( valor_string($1) , $3 ) )           { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s == %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | ID        DIFERENTE  STRINGVAL { if(  strcmp( valor_string($1) , $3 ) )           { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s != %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MAIOR      STRINGVAL { if(  strcmp( valor_string($1) , $3 ) >  0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MAIORIGUAL STRINGVAL { if(  strcmp( valor_string($1) , $3 ) >= 0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s >= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MENOR      STRINGVAL { if(  strcmp( valor_string($1) , $3 ) <  0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | ID        MENORIGUAL STRINGVAL { if(  strcmp( valor_string($1) , $3 ) <= 0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($1),yylineno); printf("%sexpresao logica: %s <= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL IGUALIGUAL ID        { if( !strcmp( $1 , valor_string($3) ) )           { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s == %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL DIFERENTE  ID        { if(  strcmp( $1 , valor_string($3) ) )           { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s != %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL MAIOR      ID        { if(  strcmp( $1 , valor_string($3) ) >  0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s >  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL MAIORIGUAL ID        { if(  strcmp( $1 , valor_string($3) ) >= 0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s >= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL MENOR      ID        { if(  strcmp( $1 , valor_string($3) ) <  0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s <  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL MENORIGUAL ID        { if(  strcmp( $1 , valor_string($3) ) <= 0 )      { $$ = true; } else { $$ = false; } add_linha(get_index($3),yylineno); printf("%sexpresao logica: %s <= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s %s",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL IGUALIGUAL STRINGVAL { if( !strcmp( $1 , $3 ) )                         { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s == %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL DIFERENTE  STRINGVAL { if(  strcmp( $1 , $3 ) )                         { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s != %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL MAIOR      STRINGVAL { if(  strcmp( $1 , $3 ) >  0 )                    { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s >  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL MAIORIGUAL STRINGVAL { if(  strcmp( $1 , $3 ) >= 0 )                    { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s >= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL MENOR      STRINGVAL { if(  strcmp( $1 , $3 ) <  0 )                    { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s <  %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | STRINGVAL MENORIGUAL STRINGVAL { if(  strcmp( $1 , $3 ) <= 0 )                    { $$ = true; } else { $$ = false; }                                    printf("%sexpresao logica: %s <= %s ✓ %s\n",azul,$1,$3,reset);     char linha[100] = ""; snprintf(linha, sizeof(linha),"\"%s\" %s \"%s\"",$1,$2,$3); escreve_arquivo(linha); }
    | ABRE_COLCHETES expl FECHA_COLCHETES E_LOGICO  ABRE_COLCHETES expl FECHA_COLCHETES { $$ = ($2 && $4);                                                     printf("%sexpresao logica: e logico (&&) ✓%s\n",azul,reset); }
    | ABRE_COLCHETES expl FECHA_COLCHETES OU_LOGICO ABRE_COLCHETES expl FECHA_COLCHETES { $$ = ($2 || $4);                                                     printf("%sexpresao logica: ou logico (||) ✓%s\n",azul,reset); }
    | NAO_LOGICO ID                                     { escreve_arquivo("!");           $$ = (!$2);                                                          printf("%sexpresao logica: negacao (!) ✓%s\n",azul,reset);    };
                                                                                                                                
att:  ID IGUAL INTVAL           { struct Valor valor = init_valor(valor); valor.valor_int = $3;    att_variavel($1,"int",valor, yylineno);    char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %i;",$1,$2,$3); escreve_arquivo(linha);     }               
    | ID IGUAL FLOATVAL         { struct Valor valor = init_valor(valor); valor.valor_float = $3;  att_variavel($1,"float",valor, yylineno);  char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %f;",$1,$2,$3); escreve_arquivo(linha);     }               
    | ID IGUAL STRINGVAL        { struct Valor valor = init_valor(valor); valor.valor_string = $3; att_variavel($1,"string",valor, yylineno); char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s \"%s\";",$1,$2,$3); escreve_arquivo(linha); }
    | ID IGUAL ID               { att_variavel_variavel($1,$3, yylineno);                                                                     char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s %s;",$1,$2,$3); escreve_arquivo(linha);     }
    | ID IGUAL expa             { att_variavel_expa($1,$3, yylineno);                                                                         escreve_arquivo($1); escreve_arquivo(" = "); escreve_arquivo(string_temp); escreve_arquivo(";");       }
    | ID MAISMAIS               { incrementa_variavel($1, yylineno);                                                                          char linha[100] = ""; snprintf(linha, sizeof(linha),"%s++;",$1);           escreve_arquivo(linha);     }
    | ID MENOSMENOS             { decrementa_variavel($1, yylineno);                                                                          char linha[100] = ""; snprintf(linha, sizeof(linha),"%s--;",$1);           escreve_arquivo(linha);     };
    
expa: ABRE_COLCHETES INTVAL   SOMA          INTVAL   FECHA_COLCHETES { $$ = (float)$2 + (float)$4;                   char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %i )",$2,$3,$4); string_temp = linha; }              
    | ABRE_COLCHETES INTVAL   SUBTRACAO     INTVAL   FECHA_COLCHETES { $$ = (float)$2 - (float)$4;                   char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %i )",$2,$3,$4); string_temp = linha; }    
    | ABRE_COLCHETES INTVAL   MULTIPLICACAO INTVAL   FECHA_COLCHETES { $$ = (float)$2 * (float)$4;                   char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %i )",$2,$3,$4); string_temp = linha; }    
    | ABRE_COLCHETES INTVAL   DIVISAO       INTVAL   FECHA_COLCHETES { $$ = (float)$2 / (float)$4;                   char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   SOMA          FLOATVAL FECHA_COLCHETES { $$ = (float)$2 + $4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   SUBTRACAO     FLOATVAL FECHA_COLCHETES { $$ = (float)$2 - $4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   MULTIPLICACAO FLOATVAL FECHA_COLCHETES { $$ = (float)$2 * $4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   DIVISAO       FLOATVAL FECHA_COLCHETES { $$ = (float)$2 / $4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   SOMA          ID       FECHA_COLCHETES { $$ = (float)$2 + valor_numerico($4);          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   SUBTRACAO     ID       FECHA_COLCHETES { $$ = (float)$2 - valor_numerico($4);          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   MULTIPLICACAO ID       FECHA_COLCHETES { $$ = (float)$2 * valor_numerico($4);          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   DIVISAO       ID       FECHA_COLCHETES { $$ = (float)$2 / valor_numerico($4);          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   SOMA          expa     FECHA_COLCHETES { $$ = (float)$2 + $4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   SUBTRACAO     expa     FECHA_COLCHETES { $$ = (float)$2 - $4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   MULTIPLICACAO expa     FECHA_COLCHETES { $$ = (float)$2 * $4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES INTVAL   DIVISAO       expa     FECHA_COLCHETES { $$ = (float)$2 / $4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %i %s %f )",$2,$3,$4); string_temp = linha; }                                                                                                                     
    | ABRE_COLCHETES FLOATVAL SOMA          INTVAL   FECHA_COLCHETES { $$ = $2 + (float)$4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL SUBTRACAO     INTVAL   FECHA_COLCHETES { $$ = $2 - (float)$4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL MULTIPLICACAO INTVAL   FECHA_COLCHETES { $$ = $2 * (float)$4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL DIVISAO       INTVAL   FECHA_COLCHETES { $$ = $2 / (float)$4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL SOMA          FLOATVAL FECHA_COLCHETES { $$ = $2 + $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL SUBTRACAO     FLOATVAL FECHA_COLCHETES { $$ = $2 - $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL MULTIPLICACAO FLOATVAL FECHA_COLCHETES { $$ = $2 * $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL DIVISAO       FLOATVAL FECHA_COLCHETES { $$ = $2 / $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL SOMA          ID       FECHA_COLCHETES { $$ = $2 + valor_numerico($4);                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL SUBTRACAO     ID       FECHA_COLCHETES { $$ = $2 - valor_numerico($4);                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL MULTIPLICACAO ID       FECHA_COLCHETES { $$ = $2 * valor_numerico($4);                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL DIVISAO       ID       FECHA_COLCHETES { $$ = $2 / valor_numerico($4);                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL SOMA          expa     FECHA_COLCHETES { $$ = $2 + $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL SUBTRACAO     expa     FECHA_COLCHETES { $$ = $2 - $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL MULTIPLICACAO expa     FECHA_COLCHETES { $$ = $2 * $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES FLOATVAL DIVISAO       expa     FECHA_COLCHETES { $$ = $2 / $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       SOMA          INTVAL   FECHA_COLCHETES { $$ = valor_numerico($2) + (float)$4;          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       SUBTRACAO     INTVAL   FECHA_COLCHETES { $$ = valor_numerico($2) - (float)$4;          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       MULTIPLICACAO INTVAL   FECHA_COLCHETES { $$ = valor_numerico($2) * (float)$4;          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       DIVISAO       INTVAL   FECHA_COLCHETES { $$ = valor_numerico($2) / (float)$4;          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       SOMA          FLOATVAL FECHA_COLCHETES { $$ = valor_numerico($2) + $4;                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       SUBTRACAO     FLOATVAL FECHA_COLCHETES { $$ = valor_numerico($2) - $4;                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       MULTIPLICACAO FLOATVAL FECHA_COLCHETES { $$ = valor_numerico($2) * $4;                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       DIVISAO       FLOATVAL FECHA_COLCHETES { $$ = valor_numerico($2) / $4;                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       SOMA          ID       FECHA_COLCHETES { $$ = valor_numerico($2) + valor_numerico($4); char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       SUBTRACAO     ID       FECHA_COLCHETES { $$ = valor_numerico($2) - valor_numerico($4); char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       MULTIPLICACAO ID       FECHA_COLCHETES { $$ = valor_numerico($2) * valor_numerico($4); char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       DIVISAO       ID       FECHA_COLCHETES { $$ = valor_numerico($2) / valor_numerico($4); char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       SOMA          expa     FECHA_COLCHETES { $$ = valor_numerico($2) + $4;                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       SUBTRACAO     expa     FECHA_COLCHETES { $$ = valor_numerico($2) - $4;                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       MULTIPLICACAO expa     FECHA_COLCHETES { $$ = valor_numerico($2) * $4;                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES ID       DIVISAO       expa     FECHA_COLCHETES { $$ = valor_numerico($2) / $4;                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %s %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     SOMA          INTVAL   FECHA_COLCHETES { $$ = $2 + (float)$4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     SUBTRACAO     INTVAL   FECHA_COLCHETES { $$ = $2 - (float)$4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     MULTIPLICACAO INTVAL   FECHA_COLCHETES { $$ = $2 * (float)$4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     DIVISAO       INTVAL   FECHA_COLCHETES { $$ = $2 / (float)$4;                          char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %i )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     SOMA          FLOATVAL FECHA_COLCHETES { $$ = $2 + $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     SUBTRACAO     FLOATVAL FECHA_COLCHETES { $$ = $2 - $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     MULTIPLICACAO FLOATVAL FECHA_COLCHETES { $$ = $2 * $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     DIVISAO       FLOATVAL FECHA_COLCHETES { $$ = $2 / $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     SOMA          ID       FECHA_COLCHETES { $$ = $2 + valor_numerico($4);                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     SUBTRACAO     ID       FECHA_COLCHETES { $$ = $2 - valor_numerico($4);                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     MULTIPLICACAO ID       FECHA_COLCHETES { $$ = $2 * valor_numerico($4);                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     DIVISAO       ID       FECHA_COLCHETES { $$ = $2 / valor_numerico($4);                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %s )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     SOMA          expa     FECHA_COLCHETES { $$ = $2 + $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     SUBTRACAO     expa     FECHA_COLCHETES { $$ = $2 - $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     MULTIPLICACAO expa     FECHA_COLCHETES { $$ = $2 * $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; }
    | ABRE_COLCHETES expa     DIVISAO       expa     FECHA_COLCHETES { $$ = $2 / $4;                                 char linha[100] = ""; snprintf(linha, sizeof(linha),"( %f %s %f )",$2,$3,$4); string_temp = linha; } ; 
    
decl: tipo ID                   { printf("%sdeclaracao simples: %s %s ✓ %s\n",azul,$1,$2,reset); struct Valor valor = init_valor(valor);                          declara_variavel($2, $1, $1,       valor, escopo, yylineno-1); char linha[100] = ""; char* tipo = $1; if(!strcmp($1,"string")){tipo = "char*";} snprintf(linha, sizeof(linha),"%s %s; ",tipo,$2); escreve_arquivo(linha);         }
    | tipo ID IGUAL INTVAL      { printf("%sdeclaracao: %s %s = %i ✓ %s\n",azul,$1,$2,$4,reset); struct Valor valor = init_valor(valor); valor.valor_int = $4;    declara_variavel($2, $1, "int",    valor, escopo, yylineno);   char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s = %i; ",$1,$2,$4); escreve_arquivo(linha); }
    | tipo ID IGUAL FLOATVAL    { printf("%sdeclaracao: %s %s = %f ✓ %s\n",azul,$1,$2,$4,reset); struct Valor valor = init_valor(valor); valor.valor_float = $4;  declara_variavel($2, $1, "float",  valor, escopo, yylineno);   char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s = %f; ",$1,$2,$4); escreve_arquivo(linha); }
    | tipo ID IGUAL STRINGVAL   { printf("%sdeclaracao: %s %s = %s ✓ %s\n",azul,$1,$2,$4,reset); struct Valor valor = init_valor(valor); valor.valor_string = $4; declara_variavel($2, $1, "string", valor, escopo, yylineno);   char linha[100] = ""; snprintf(linha, sizeof(linha),"char* %s = \"%s\"; ",$2,$4); escreve_arquivo(linha); }
    | tipo ID IGUAL ID          { printf("%sdeclaracao: %s %s = %s ✓ %s\n",azul,$1,$2,$4,reset); char* tipo_valor = tabela_simbolos[get_index($4)].tipo; declara_variavel($2, $1, tipo_valor, tabela_simbolos[get_index($4)].valor, escopo, yylineno); char linha[100] = ""; snprintf(linha, sizeof(linha),"%s %s = %s; ",$1,$2,$4); escreve_arquivo(linha); }
    | tipo ID IGUAL expa        { printf("%sdeclaracao: %s %s = %f ✓ %s\n",azul,$1,$2,$4,reset); declara_variavel_expa($2, $1, $4, escopo, yylineno); escreve_arquivo($1); escreve_arquivo(" "); escreve_arquivo($2); escreve_arquivo(" = "); escreve_arquivo(string_temp); escreve_arquivo(";"); }; // snprintf corrompe o valor da variavel string_temp...?

tipo: INT       {$$ = $1;}
    | FLOAT     {$$ = $1;}
    | STRING    {$$ = $1;};

linha_opt: QUEBRA_LINHA
    | %empty;

%%

void init_arquivo(){
    FILE *fp;
    fp = fopen("test.c", "w");
    if(fp == NULL) {
        printf("file can't be opened\n");
        exit(1);
    }
    fprintf(fp,"#include <stdio.h>\n");
    fclose(fp);
}

void escreve_arquivo(char* cmd){
    FILE *fp;
    fp = fopen("test.c", "a");
    if(fp == NULL) {
        printf("file can't be opened\n");
        exit(1);
    }
    fprintf(fp,"%s",cmd);
    fclose(fp);
}

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

float valor_numerico(char* nome){ //retorna valor numerico da variavel
    if(existe_variavel(nome,1)){
        int i = get_index(nome);
        add_linha(get_index(nome),yylineno);
        if(!tipo_variavel(nome,"string")){ 
            float num = (float)(tabela_simbolos[get_index(nome)].valor.valor_int + tabela_simbolos[get_index(nome)].valor.valor_float); 
        }else{
            printf("%sErro, comparacao de tipos incompativeis para variavel %s: string != int/float %s\n",vermelho,nome,reset);
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
                    printf("add %s ✓\n", nome);
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
                    printf("add %s ✓\n", nome);
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
            printf("%satt %s%s ✓\n",roxo,nome,reset);
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
            printf("%satt %s%s ✓\n",roxo,nome,reset);
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

char* tipo_print(char* nome){
    if(tipo_variavel(nome,"int")){
        return "%i";
    }
    if(tipo_variavel(nome,"float")){
        return "%f";
    }
    if(tipo_variavel(nome,"string")){
        return "%s";
    }
    return "";
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

void compila_codigo(){
    printf("\nnumero de erros: %i\n",numero_erros);
    if(numero_erros == 0){
        printf("executando codigo compilado:\n\n");
        system("sudo gcc ./test.c -o test;");
        system("./test");
    }
}

int main(int argc, char **argv){
    printf("----------------------------------------------------------------------------------\n");
    printf("Iniciando...\n\n");
    return yyparse ( );
}

void yyerror (char *s) {
    printf("Linha: %i\n%s\n",yylineno,s);
    fprintf (stderr, "%s",s);
    numero_erros++;
} 
