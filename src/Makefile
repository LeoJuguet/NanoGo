
EXE=_build/default/main.exe

TESTS=../tests/*.go

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ngoc

test: $(EXE) $(TESTS)
	echo test

%.go:
	./ngoc --debug --type-only $*.go
	#gcc -g -no-pie $*.s -o $*
	#$*
	#go run $*.go

#export-%:
#	cp test.go ../tests/exec/$*.go
#	go run test.go > ../tests/exec/$*.out

.PHONY: clean
clean:
	dune clean
cleantest:
	rm -f test.png test_ast.dot
