%{
extern void yyerror();  //void yyerror (char *s);
extern int yylex();
extern int yylineno;
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
struct Valor{ int valor_int; float valor_float; char* valor_string; };
struct Map{ int index; char* nome; char* tipo; struct Valor valor; };
#define TAMANHO_TABELA 100
struct Map tabela_simbolos[TAMANHO_TABELA];
void init_tabela();
void print_tabela();
void declara_variavel();
void att_variavel();
void print_variavel();
int existe_variavel();
int index_variavel();
extern char* verde;
extern char* reset;
extern char* azul;
extern char* roxo;
extern char* vermelho;
%} 

//%skeleton "lalr1.cc"
//%define parser_class_name {test1_parser}
//%define api.token.constructor
//%define api.value.type variant
//%define parse.assert
//%define parse.error verbose
//%locations

//%code requires {
//#include <map>
//#include <list>
//#include <vector>
//#include <string>
//#include <iostream>
//#include <algorithm>
//}

//%define parse.lac full
%define parse.error verbose

%union {
    char* id;           //nome da variavel
    int   valorint;
    float valorfloat;
    char* valorchar;
    char* tipovar;    
}

%token ABRE_PARENTESES FECHA_PARENTESES ABRE_CHAVE FECHA_CHAVE ABRE_COLCHETES FECHA_COLCHETES QUEBRA_LINHA
%token ASPAS_SIMPLES ASPAS_DUPLAS PONTO_VIRGULA VIRGULA 
%token IF ELSE FOR WHILE SWITCH OUT
%token <id> ID
%token <id> INT <valorint> INTVAL <id> FLOAT <valorfloat> FLOATVAL <id> STRING <valorchar> STRINGVAL
%token <id> IGUAL IGUALIGUAL DIFERENTE MAIOR MAIORIGUAL MENOR MENORIGUAL
%token SOMA SUBTRACAO MULTIPLICACAO DIVISAO MAISMAIS MENOSMENOS
%type <id> tipo

// %type <intval> termo

%%

programa: /* { ++ctx; } */{ init_tabela(); } cmd {printf("%sprograma ✓%s\n",verde, reset); print_tabela(); }/* { -- ctx; } */;
    
cmd:  cmd if            { printf("%scmd if ✓     %s\n",verde, reset); }
    | cmd for           { printf("%scmd for ✓    %s\n",verde, reset);}
    | cmd while         { printf("%scmd while ✓  %s\n",verde, reset);}
    | cmd SWITCH
    | cmd funcao        { printf("%scmd funcao ✓ %s\n",verde, reset); }
    | cmd out           { printf("%scmd out ✓    %s\n",verde, reset); }
    | cmd att         /*{ printf("%satribuicao ✓ %s\n",verde, reset); }*/
    | cmd decl          { printf("%sdeclaracao ✓ %s\n",verde, reset); }
    | cmd QUEBRA_LINHA
    | %empty;
    
if: IF ABRE_COLCHETES expl FECHA_COLCHETES linha_opt ABRE_COLCHETES cmd FECHA_COLCHETES else;
    
else: ELSE linha_opt if
    | ELSE linha_opt ABRE_COLCHETES cmd FECHA_COLCHETES
    | %empty;
    
for: FOR ABRE_COLCHETES att PONTO_VIRGULA expl PONTO_VIRGULA att FECHA_COLCHETES ABRE_COLCHETES cmd FECHA_COLCHETES;

while: WHILE ABRE_COLCHETES expl FECHA_COLCHETES ABRE_COLCHETES cmd FECHA_COLCHETES;

funcao: tipo ID ABRE_COLCHETES declaracao_parametros FECHA_COLCHETES linha_opt ABRE_COLCHETES cmd FECHA_COLCHETES;

out: OUT ABRE_COLCHETES STRINGVAL FECHA_COLCHETES       { printf("out: >%s<\n",$3);};

declaracao_parametros: parametros
    | %empty;
    
parametros: parametros ',' ID
    | ID;
    
expl: termo opl termo;    //{ printf("expl %i\n", $1); };

termo: ID           //{$$ = symbolVal($1);}
    | INTVAL        {printf("valor int: %i\n",$1);}//{$$ = $1;}
    | FLOATVAL      {printf("valor float: %f\n",$1);}
    | STRINGVAL;

opl: IGUALIGUAL | DIFERENTE | MAIOR | MAIORIGUAL | MENOR | MENORIGUAL { printf("opl\n"); };// precedencia de operadores desnecessaria devido ao uso obrigatorio de colchetes

//opa: SOMA | SUBTRACAO | MULTIPLICACAO | DIVISAO;
    
att:  ID IGUAL INTVAL           { struct Valor valor; valor.valor_int = $3; valor.valor_float  = 0;  valor.valor_string = ""; att_variavel($1,"int",valor); }
    | ID IGUAL FLOATVAL         { struct Valor valor; valor.valor_int = 0;  valor.valor_float  = $3; valor.valor_string = ""; att_variavel($1,"float",valor); }
    | ID IGUAL STRINGVAL        { struct Valor valor; valor.valor_int = 0;  valor.valor_float  = 0;  valor.valor_string = $3; att_variavel($1,"string",valor); }
    | ID IGUAL ID               { printf("atribuicao: %s \n",$1);}
    | ID IGUAL expa             { printf("atribuicao: %s \n",$1);}
    | ID MAISMAIS               { printf("atribuicao: %s \n",$1);}
    | ID MENOSMENOS             { printf("atribuicao: %s \n",$1);};
    
expa: ABRE_COLCHETES expl                      FECHA_COLCHETES
    | ABRE_COLCHETES expa2 SOMA          expa2 FECHA_COLCHETES    //{$$ = $2 + $4;}
    | ABRE_COLCHETES expa2 SUBTRACAO     expa2 FECHA_COLCHETES    //{$$ = $2 - $4;}
    | ABRE_COLCHETES expa2 MULTIPLICACAO expa2 FECHA_COLCHETES    //{$$ = $2 * $4;}
    | ABRE_COLCHETES expa2 DIVISAO       expa2 FECHA_COLCHETES;    //{$$ = $2 / $4;}
    
expa2: expa
    | ID;
    
decl: tipo ID                   { printf("%sdeclaracao simples: %s %s %s\n",azul,$1,$2,reset); struct Valor valor; valor.valor_int = 0;  valor.valor_float  = 0;  valor.valor_string = ""; declara_variavel($2, $1, valor); }
    | tipo ID IGUAL INTVAL      { printf("%sdeclaracao: %s %s = %i %s\n",azul,$1,$2,$4,reset); struct Valor valor; valor.valor_int = $4; valor.valor_float  = 0;  valor.valor_string = ""; declara_variavel($2, $1, valor); }
    | tipo ID IGUAL FLOATVAL    { printf("%sdeclaracao: %s %s = %f %s\n",azul,$1,$2,$4,reset); struct Valor valor; valor.valor_int = 0;  valor.valor_float  = $4; valor.valor_string = ""; declara_variavel($2, $1, valor); }
    | tipo ID IGUAL STRINGVAL   { printf("%sdeclaracao: %s %s = %s %s\n",azul,$1,$2,$4,reset); struct Valor valor; valor.valor_int = 0;  valor.valor_float  = 0;  valor.valor_string = $4; declara_variavel($2, $1, valor); }
    | tipo ID IGUAL ID          { printf("%sdeclaracao: %s %s = %s %s\n",azul,$1,$2,$4,reset); declara_variavel($2, $1, tabela_simbolos[index_variavel($4)].valor); }
    | tipo ID IGUAL expa        { printf("%sdeclaracao: \n",$2); };        //{ printf("declaracao + atribuicao\n"); };
// dectipo l_aux: ID                { printf("asdasd\n");}
//     | att;

tipo: INT       {$$ = $1;}
    | FLOAT     {$$ = $1;}
    | STRING    {$$ = $1;};

linha_opt: QUEBRA_LINHA
    | %empty;

%%

void init_tabela(){
    printf("Iniciando tabela...\n");
    for(int i = 0; i < TAMANHO_TABELA; i++){
        tabela_simbolos[i].nome = "";
    }
}

int existe_variavel(char* nome){
    if(index_variavel(nome) != -1){
        return 1;
    }
    return 0;
}
int index_variavel(char* nome){
    for(int i = 0; i < TAMANHO_TABELA; i++){
        if(!strcmp(tabela_simbolos[i].nome, nome)){
            return i; // variavel encontrada neste indice
        }
    }
    return -1; //variavel nao encontrada na tabela
}

void declara_variavel(char* nome, char* tipo, struct Valor val){ //busca nome da variavel na lista, se nao existir, adiciona
    if(index_variavel(nome) == -1){
        for(int i = 0; i < TAMANHO_TABELA; i++){
            if(!strcmp(tabela_simbolos[i].nome, "")){
                tabela_simbolos[i].nome = nome;
                tabela_simbolos[i].tipo = tipo;
                tabela_simbolos[i].valor = val;
                break; 
            }        
        }
    }
    printf("%s",roxo);
    printf("add %s \n", nome);
    printf("%s",reset);
}

void att_variavel(char* nome, char* tipo, struct Valor val){
    int i = index_variavel(nome);
    if(i != -1){
        if(!strcmp(tabela_simbolos[i].tipo, tipo)){
            tabela_simbolos[i].valor = val;
            printf("atribuicao: %s\n",nome);
        }else{
            printf("%sErro, atribuicao de tipos incompativeis para variavel %s: %s != %s %s\n",vermelho,nome,tabela_simbolos[i].tipo,tipo,reset);
            yyerror("Erro, atribuicao de tipos incompativeis");
        }
    }else{
        printf("%sErro, variavel %s nao declarada%s\n",vermelho,nome,reset);
        yyerror("Erro, variavel nao declarada");
    }
}

void print_variavel(char* nome){
    int i = index_variavel(nome);
    printf("variavel: %s, tipo: %s ",tabela_simbolos[i].nome, tabela_simbolos[i].tipo); 
    if(!strcmp(tabela_simbolos[i].tipo, "int"))   { printf("valor: %i\n",tabela_simbolos[i].valor.valor_int);    }            
    if(!strcmp(tabela_simbolos[i].tipo, "float")) { printf("valor: %f\n",tabela_simbolos[i].valor.valor_float);  }            
    if(!strcmp(tabela_simbolos[i].tipo, "string")){ printf("valor: %s\n",tabela_simbolos[i].valor.valor_string); }            
}

void print_tabela(){
    for(int i = 0; i < TAMANHO_TABELA; i++){
        if(strcmp(tabela_simbolos[i].nome, "")){
            print_variavel(tabela_simbolos[i].nome);
        }
    }
}



int main(int argc, char **argv){
    printf("Iniciando...\n\n");
    return yyparse ( );    
}

void yyerror (char *s) {fprintf (stderr, "%s\nLinha: ~%i\n", s, yylineno);} 
    
// bison -d test1.y --report=all
    
