package main

import "fmt"

func fact(n int) int {
	if n == 1 {
		return 1
	}
	return n * fact(n-1)
}

func main() {
	var want = 120
        got := fact(5)
	if  got != want {
	   fmt.Print("got %d, want %d", got, want)
	}
}
