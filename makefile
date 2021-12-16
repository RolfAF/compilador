all: compilador
	#./compilador;
	./compilador < EXEMPLO_GRAMATICA.TXT;

compilador: lex.yy.c y.tab.c
	gcc -g lex.yy.c compilador.tab.c -o compilador;

lex.yy.c: y.tab.c compilador.l
	flex compilador.l

y.tab.c: compilador.y
	bison -d compilador.y --report=all

clean: 
	rm -rf lex.yy.c compilador.tab.c compilador.tab.h compilador compilador.dSYM
