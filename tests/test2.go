package main
import "fmt";

type Test struct {
     a int;
     b *Test2
     }

type Test2 struct {
     x *Test
     s string
     j Test
}

func f(a int, b int, c string)(int, string){
     fmt.Print(a,b,c);
     return a,c
}


func g()(int, bool,string){
     return 5,true,"oh"
}

func main(){
     var a, b = f(5,6,"faux"), 5;
     var c Test;
     var d Test2;
     var e , f int ;
     var g string;
     var w,x = f(g());
     (b+b), g= f(4,7,"oh");
     d.s = "hello";
     c.b = &d;
     c.b.x = &c;
}
