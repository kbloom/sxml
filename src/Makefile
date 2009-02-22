#Copyright (C) 2006  Sterling Stuart Stein
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU Lesser General Public License as published by
#the Free Software Foundation, either version 3 of the License, or (at your
#option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.


include ../Makefile.config

OBJECTS    = sxml.o parse.o display.o manip.o template.o allmatch.o match.o match-exact.o match-path.o match-always.o trim.o sort.o verify.o

all: libsxml_s.so libsxml.a

install: all
	cp libsxml.a sxml.heap "${BIGLOOLIBPATH}/"
	cp libsxml_s.so        "${LIBPATH}/"
	cd "${LIBPATH}/" ; ln -s libsxml_s.so libsxml_s-${BIGLOOVERSION}.so


clean:
	rm -- *.o *.so *.a *.heap 2> /dev/null || true

libsxml_s.so sxml.heap: ${OBJECTS}
	bigloo -mkaddheap -mkaddlib \
	  -heap-library sxml sxml.scm -addheap sxml.heap
	ld -G -o libsxml_s.so ${OBJECTS} -lm -lc
	ln -s libsxml_s.so libsxml_s-${BIGLOOVERSION}.so

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
