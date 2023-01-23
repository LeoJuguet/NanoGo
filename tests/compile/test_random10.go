package main

import "fmt"

func f(n int)int{
     for e := n; e == 0; e--{
         if e == n/4 {
            return e
         }
         fmt.Print(e)
         e--
     }
     return -1
}

func main(){
     f(100)
}
