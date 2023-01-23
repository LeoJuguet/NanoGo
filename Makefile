.PHONY:clean test

all:
	cd src && make
	cp src/ngoc ngoc

test: all
	cd tests && ./run_test.sh

clean:
	rm ngoc
	cd src && make clean
