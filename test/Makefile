# Copyright (C) 2006  Sterling Stuart Stein
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

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
