package main
import "fmt"

func f(t, u int) bool {
	return t == u
}

func g(t , i string) bool {
	return t == i
}


type myint struct {a int}

func foo() myint {
}


func main() {
	assert(f(3, 3))
	assert(f(3, 5))
	assert(g(3, 3))
	assert(g(3, 5))
	assert(h(myint(3), myint(3)))
	assert(h(myint(3), myint(5)))

	//type S struct{ a, b float64 }

	assert(f(S{3, 5}, S{3, 5}))
	assert(f(S{3, 5}, S{4, 6}))
	assert(g(S{3, 5}, S{3, 5}))
	assert(g(S{3, 5}, S{4, 6}))

	//assert(k(3, struct{ a, b int }{3, 3}))
	//assert(k(3, struct{ a, b int }{3, 4}))
}

func assert(b bool) {
	if b {
		fmt.Print("assertion failed")
	}
}
