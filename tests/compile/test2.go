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

func test_random9(){
     1
}

func g()(int, int,string){
     var gg = 0;
     if 2 > 3 {
        var gg = "non";
        fmt.Print(gg);
     }
     gg = 5;
     fmt.Print(gg);
     return 5,4,"oh"
}

func test_wild(o *int) *int{
     return o;
}
func noo(_ int) int {
    var _ int
    _++
    //var b = _+_
    return 5
}

func main(){
     var a, b = f(5,6,"faux"), 5;
     var c Test;
     var d Test2;
     &_;
     var ntest = new(Test2);
     var e , f int ;
     var g string;
     var _,_ = f(2,5+6,"a");
     var p, _= f(g());
     var oo = test_wild(nil);
     _, g = f(2,9,"no");
     g = "oh";
     d.s = "hello";
     c.b = &d;
     f = 5;
     e = 6;
     c.b.x = &c;

     //Utilisation de variables
     _,_ = a, oo;
     fmt.Print(g,f,e,ntest,p,b)
}
