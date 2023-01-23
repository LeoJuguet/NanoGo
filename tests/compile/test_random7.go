package main

type s struct{
     a int
}

func f(){}

func h() (int,int){return 0,0}

func g() (int, int){
     return h()
}

func main(){
     var i = 0;
     i++;
     _ = i;
     var e *int;
     i = *e;
     var st *s;
     i = st.a;
     return
}
