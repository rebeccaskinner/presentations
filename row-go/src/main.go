package main

import (
	"fmt"
)

//go:generate ./lenses
//go:generate ./rows

type Person struct {
	_personName    string
	_personAddress string
	_personAge     int
}

func (p Person) Name() string { return p._personName }
func (p Person) Age() int     { return p._personAge }

type HasName interface {
	Name() string
}
type HasAge interface {
	Age() int
}

func SayHappyBirthday(t interface {
	HasName
	HasAge
}) {
	fmt.Printf("Happy %d birthday, %s\n", t.Age(), t.Name())
}

func ReverseMapInt(f func(int) int, ints []int) []int {
	out := []int{}
	for _, i := range ints {
		out = append([]int{f(i)}, out...)
	}
	return out
}

func main() {
}
