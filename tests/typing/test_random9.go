package main

func _() int{return 0}

type _ struct{}

func main(){
     _ := new(_);
     _();
     &_
     _ = _()
}
