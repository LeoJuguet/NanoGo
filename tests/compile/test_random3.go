package main

import "fmt"

func intf() int {
    return 1
}

func boolf() bool {
    return true
}

func stringf() string {
    return "hello"
}

func twovalsf() (int, int) {
    return 1, 2
}

func main() {
    if boolf() && intf()+intf() == 2 && twovalsf() == twovalsf() {
        if (boolf() == true) && boolf() && boolf() {
            fmt.Print(stringf())
        }
    }
}
