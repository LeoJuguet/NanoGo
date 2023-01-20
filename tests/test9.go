package main

import "fmt"

func test(a int) (int,int){
	return a, 2*a
}


func main(){
	var a,b = test(4);
	fmt.Print(a,b);
}
