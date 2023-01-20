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

func main(){
	var s = new(testType);
	var g testType;
	s.a = 600;
	s.b = 5;
	s.c = "ceci est du texte";
	s.d = &g;
	fmt.Print("ok",s,"ah");
}
