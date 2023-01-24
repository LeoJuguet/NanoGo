package main

import "fmt"

func f(ptr *int) int{
	*ptr++
	return *ptr
}

func main(){
	var a = 0
	fmt.Print(f(&a),f(&a),f(&a))
}
