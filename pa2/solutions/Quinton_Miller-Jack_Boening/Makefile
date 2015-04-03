PROGRAM = pa2
CFILES = frontend.c ast.c
HEADERS = parser.h lexer.h ast.h
YFILE = parser.y
LFILE = lexer.l

CC = gcc
CFLAGS = -std=gnu99 -g -Wall `pkg-config --cflags glib-2.0`
LDLIBS = `pkg-config --libs glib-2.0`
LEX = flex
YACC = bison

YFLAGS = -v --defines=$(YFILE:%.y=%.h) -o y.tab.c
LFLAGS = --header-file=$(LFILE:%.l=%.h)

%.o: %.c $(HEADERS)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@

$(PROGRAM): $(YFILE:%.y=%.o) $(LFILE:%.l=%.o) $(CFILES:%.c=%.o)
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

.PHONY: clean
clean:
	-rm -f $(YFILE:%.y=%.o) $(YFILE:%.y=%.c) $(YFILE:%.y=%.h)
	-rm -f $(LFILE:%.l=%.o) $(LFILE:%.l=%.c) $(LFILE:%.l=%.h)
	-rm -f $(CFILES:%.c=%.o)
	-rm -f $(PROGRAM)
