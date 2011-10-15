cc = clang
cflags = -std=c99 -Wall -pedantic -O3 -c
oflags = -o

c4 : connect4.o
	${cc} ${oflags} c4 connect4.o
connect4.o : connect4.c
	${cc} ${cflags} connect4.c
clean :
	rm -f c4 connect4.o
