include ../Makefile.config

INCLUDES = -I ../src
LIB      = ../src/libsxml.a
export LD_LIBRARY_PATH = ../src:.
export BIGLOOLIB = ${BIGLOOLIBPATH}:../src:.

all: test-sxml

clean:
	rm *.o test-sxml 2> /dev/null || true

test-sxml: test-sxml.o
	bigloo -o test-sxml test-sxml.o

test-sxml.o: test-sxml.scm
	bigloo -c test-sxml.scm

run_test: test-sxml
	./test-sxml
