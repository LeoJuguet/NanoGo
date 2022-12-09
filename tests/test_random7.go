package main

type s struct{
     a int
}

func main(){
     var i = 0;
     i++;
     _ = i;
     var e *int;
     i = *e;
     var st *s;
     i = nil.a;
}
