package main

import "fmt"

type S struct{
	a int
	b int
	c string
}

func createS(a int,b int,c string) S {
	var s S;
	s.a,s.b,s.c = a,b,c
	return s
}

func printS(s S){
	fmt.Print("a : ", s.a,"\nb : ", s.b, "\nc : ", s.c, "\n")
}

func main(){
	var a = new(int);
	*a = 10;
	fmt.Print(*a);
	var s = createS(5,10,"oui")
	fmt.Print(s,"\n")
	printS(s);
	printS(createS(100,48,"ok"))
}
