package main

import "fmt"

func main(){
	if "test espace" == "test espace"{
		fmt.Print("ok\n")
	}else{
		fmt.Print("not ok\n")
	}
	if "test" == "tests"{
		fmt.Print("not ok\n")
	}else{
		fmt.Print("ok\n")
	}
	if "test espace" != "test espace"{
		fmt.Print("not ok\n")
	}else{
		fmt.Print("ok\n")
	}
	if "test" != "tests"{
		fmt.Print("ok\n")
	}else{
		fmt.Print("not ok\n")
	}

}
