include ../Makefile.config

INCLUDES = -I ../src
LIB      = ../src/libsxml.a
export LD_LIBRARY_PATH = ../src:.
export BIGLOOLIB = ${BIGLOOLIBPATH}:../src:.

all: test-sxml apply-sxml

clean:
	rm *.o test-sxml apply-sxml 2> /dev/null || true

run_test: test-sxml
	cat simple.xml | ./test-sxml

run_apply: apply-sxml
	cat simple.xml | ./apply-sxml

test-sxml: test-sxml.o
	bigloo -o test-sxml test-sxml.o

test-sxml.o: test-sxml.scm
	bigloo -c test-sxml.scm

apply-sxml: apply-sxml.o
	bigloo -o apply-sxml apply-sxml.o

apply-sxml.o: apply-sxml.scm
	bigloo -c apply-sxml.scm
