BISON = /opt/homebrew/opt/bison/bin/bison
FLEX = /opt/homebrew/opt/flex/bin/flex

all: lexer parser

parser:
	${BISON} -d -t -v --language=c++ parser.y

lexer:
	${FLEX} -+ lexer.l

clean:
	rm *.tab.c *.tab.h *.yy.c

.phony: all lexer parser clean
