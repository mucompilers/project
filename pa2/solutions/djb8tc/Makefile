pa2: driver.c assignment2.lex assignment2.y
	bison -d assignment2.y -v
	flex assignment2.lex
	gcc -o pa2 driver.c assignment2.tab.c lex.yy.c `pkg-config --cflags glib-2.0` `pkg-config --libs glib-2.0` -g