package main

import "fmt"

type S struct{
	a int
	b int
	c string
}

func main(){
	var s S;
	var t S;
	s.a = 5;
	s.b = 3;
	s.c ="oui";
	t.a, t.b, t.c = 5, 3,"oui";
	if s == t{
		fmt.Print("ok\n")
	}else{
		fmt.Print("erreur\n")
	}
	if s != t{
		fmt.Print("erreur\n")
	}else{
		fmt.Print("ok\n")
	}
	t.b = 4;
	fmt.Print(t,"\n")
	if s != t{
		fmt.Print("ok\n")
	}else{
		fmt.Print("erreur\n")
	}
	if s == t{
		fmt.Print("erreur\n")
	}else{
		fmt.Print("ok\n")
	}
	var g S;
	g = s;
	fmt.Print(g,"\n")
}
