package main

import "fmt"

type testType struct {
    a int;
	b int;
	c string;
	d *testType
}

func test(b int) int {
	return b+1
}

func factorielle_r(n int) int{
	if(n <= 1){
		return 1
	}
	return n * factorielle_r(n-1)
}

func factorielle_i(n int) int{
	var result = 1
	for i := 1; i <= n; i++{
		result = result * i
	}
	return result
}

func somme(a int, b int, c int) (int,int,int,int){
	return a+1,b+1,c+1,a+b+c
}

func f(a int,b int) (int,int){
	return a-b, a+b
}

func create_int() *int{
	var t = new(int);
	*t = 42
	return t;
}

type S struct {
    a int
	b int
}

type S_in struct {
	s string
	st S
	sp *S
}

type R struct {
	s string
	st S_in
}

func testS() {
	var ns = new(S);
	ns.a = 5;
	ns.b = 42;
	//fmt.Print(ns,"\n")
	var nns S_in;
	nns.s = "Ceci est un test"
	nns.st = *ns
	nns.sp = ns
	fmt.Print(nns,"\n")
	var r R;
	r.s = "autre test de truc"
	r.st = nns
	fmt.Print(r,"\n")
}

func test_assign_multiple(){
	var a,b = 5,10;
	a,b = b,a;
	fmt.Print(a,b,"\n");
	a,b = f(a,b);
	fmt.Print(a,b,"\n")
}

func test_assign__(){
	var _, a = f(5,6);
	_,_ = 5,5;
	a,_=f(a,a);
	fmt.Print(a,"\n");
}

func test_op(){
	if 5 > 6{
		fmt.Print("> : erreur \n")
	}
	if 6 > 5{
		fmt.Print("> : ok \n")
	}
	if 5 > 5{
		fmt.Print("> : erreur \n")
	}
	if 5 >= 6{
		fmt.Print(">= : erreur \n")
	}
	if 6 >= 5{
		fmt.Print(">= : ok \n")
	}
	if 5 >= 5{
		fmt.Print(">= : ok \n")
	}
	if 5 < 6{
		fmt.Print("< : ok \n")
	}
	if 6 < 5{
		fmt.Print("< : erreur \n")
	}
	if 5 < 5{
		fmt.Print("< : erreur \n")
	}
	if 5 <= 6{
		fmt.Print("<= : ok \n")
	}
	if 6 <= 5{
		fmt.Print("<= : erreur \n")
	}
	if 5 <= 5{
		fmt.Print("<= : ok \n")
	}
	if 5 == 6{
		fmt.Print("== : erreur \n")
	}
	if 6 == 5{
		fmt.Print("== : erreur \n")
	}
	if 5 == 5{
		fmt.Print("== : ok \n")
	}
	if 5 != 6{
		fmt.Print("!= : ok \n")
	}
	if 6 != 5{
		fmt.Print("!= : ok \n")
	}
	if 5 != 5{
		fmt.Print("!= : erreur \n")
	}else{
		fmt.Print("else : ok \n")
	}

}

func test_for(){
	for i := 0; i < 10; i++{
		fmt.Print(i,"\n")
	}
}

func test_while(){
	var i = 0;
	for i < 10{
		fmt.Print(i,"\n")
		i++
	}
}

func main(){
	facti := factorielle_i(5);
	factr := factorielle_r(5);
	fmt.Print(facti, "\n", factr, "\n");
	fmt.Print(somme(5,10,15));
	fmt.Print("\n");
	fmt.Print(f(5,10))
	fmt.Print("\n");
	fmt.Print(f(f(5,10)))
	fmt.Print("\n");

	var t = create_int();
	fmt.Print("Entier ", *t, " a la position ", t, "\n");
	var s S;
	s.a = 10;
	s.b = 100;
	fmt.Print(s,"\n");
	testS();
	fmt.Print(nil, "\n")
	test_assign_multiple()
	test_assign__()
	test_op()
	test_for()
}
