include ../Makefile.config

OBJECTS    = sxml.o parse.o display.o manip.o template.o allmatch.o match.o match-exact.o match-path.o match-always.o trim.o sort.o verify.o

all: libsxml_s.so libsxml.a

install: all
	cp libsxml.a sxml.heap "${BIGLOOLIBPATH}/"
	cp libsxml_s.so        "${LIBPATH}/"

clean:
	rm *.o libsxml_s.so libsxml.a sxml.heap 2> /dev/null || true

libsxml_s.so sxml.heap: ${OBJECTS}
	bigloo -mkaddheap -mkaddlib \
	  sxml.scm -addheap sxml.heap
	ld -G -o libsxml_s.so ${OBJECTS} -lm -lc

libsxml.a: ${OBJECTS}
	ar qcv libsxml.a ${OBJECTS}
	ranlib libsxml.a


sxml.o: sxml.scm
	bigloo -c sxml.scm

parse.o: parse.scm
	bigloo -c parse.scm

display.o: display.scm manip.o
	bigloo -c display.scm

manip.o: manip.scm
	bigloo -c manip.scm

template.o: template.scm manip.o allmatch.o
	bigloo -c template.scm

allmatch.o: allmatch.scm match.o match-exact.o match-path.o match-always.o
	bigloo -c allmatch.scm

match.o: match.scm
	bigloo -c match.scm

match-exact.o: match-exact.scm match.o manip.o
	bigloo -c match-exact.scm

match-path.o: match-path.scm match.o manip.o macro.sch
	bigloo -c match-path.scm

match-always.o: match-always.scm match.o
	bigloo -c match-always.scm

trim.o: trim.scm manip.o template.o
	bigloo -c trim.scm

sort.o: sort.scm manip.o
	bigloo -c sort.scm

verify.o: verify.scm manip.o
	bigloo -c verify.scm
