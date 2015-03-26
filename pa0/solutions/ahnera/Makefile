PROGRAM = pa0lex
CFILES = frontend.c
HEADERS = lexer.h
LFILE = lexer.l

CC = gcc
CFLAGS = -Wall
LEX = flex

# Make all headers a dependency to everything.
%.o: %.c $(HEADERS)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@

$(PROGRAM): $(LFILE:.l=.o) $(CFILES:.c=.o) $(HEADERS)
	$(CC) $(LDFLAGS) $(filter %.o, $^) $(LDLIBS) -o $@

run:
	./$(PROGRAM)

clean:
	-rm -f $(LFILE:.l=.o) $(LFILE:.l=.c)
	-rm -f $(CFILES:.c=.o)
	-rm -f $(PROGRAM)
