package main

type B1 interface{}
type A1 interface {
	B1
}
type B2 interface{}
type A2 interface {
	B2
}
type F1 interface {
	f1(B1) A2
}
type F2 interface {
	f2(A1) B2
}

func f2_from_f1(b1 A1) B2 {}
