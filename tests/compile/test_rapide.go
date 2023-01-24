package main

import "fmt"

type S struct{
	a int
	b int
}

func main(){
	var s,t S;
	s.a, s.b, t.a ,t.b = 5,3,3,2
	fmt.Print(s != t,"\n")
	fmt.Print( 1 > 5)
	fmt.Print( 1 < 5 )
}
