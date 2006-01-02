all:
	cd src; make
	cd example; make

install:
	cd src; make install

clean:
	cd src; make clean
	cd example; make clean
