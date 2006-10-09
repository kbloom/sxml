all:
	cd src; make
	cd test; make

install:
	cd src; make install

clean:
	cd src; make clean
	cd test; make clean
