package main

import (
	"fmt"
)

func f(b *int){
	b++;
	b
}

func main(){
	b := 0;
	fmt.Print(b,f(&b),f(&b),b);
}
