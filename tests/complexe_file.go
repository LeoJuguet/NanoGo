package main

import "fmt"

type List struct {
	next *List
	val  int
}

func create_list(next *List,val int) *List{
     l := new(List);
     l.next = next;
     l.val = val;
     return l;
}

func Largest(l *List) int {
	var max int
	for p := l; p != nil; p = p.next {
		if p.val > max {
			max = p.val
		}
	}
	return max
}

type ListNum struct {
	next *ListNum
	val  int
}


func ClippedLargest(l *ListNum) int {
	var max int
	for p := l; p != nil; p = p.next {
		if p.val > max && p.val < 5 {
			max = p.val
		}
	}
	return max
}

func main() {
        i3 := create_list(nil,1);
        i2 := create_list(i3,3);
        i1 := create_list(i2,2);
        got, want := Largest(i1), 3;
	if got != want {
		fmt.Print(got, want)
	}
        var j3,j2,j1 *ListNum;
        j3.next = nil;
        j3.val = 1;
        j2.next = j3;
        j2.val = 50;
        j1.next = j2;
        j1.val = 2;
        got, want = ClippedLargest(j1), 2
	if  got != want {
		fmt.Print( got, want)
	}
}
