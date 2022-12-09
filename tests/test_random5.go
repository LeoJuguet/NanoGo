package main
import "fmt"

func main(){
     var a int;

     if &a == nil {
        fmt.Print("equal")
     }else{
        fmt.Print("not equal")
     }
}

//not equal
