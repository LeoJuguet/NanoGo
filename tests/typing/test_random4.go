package main


type s1 struct{
     s1 *s1
     s2 s2
     i int
     b bool
     s string
}

type s2 struct{
     s3 s3
     s1 *s1
}

type s3 struct{
     s1 *s1
     s2 *s2
}

func f(a,b int) (int,string) {
     return 0,""
}

func g(a int, b string) (int, int){
     return a, a
}



func main(){
     var g = f(5,5);


}
