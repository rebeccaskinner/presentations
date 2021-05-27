.PHONY: all
all: static-fib-c dynamic-fib-c static-fib-hs dynamic-fib-hs libfib-overlay.so

libfib.o:
	nasm -felf64 libfib.s -o libfib.o

libfib.so: libfib.o
	ld -shared $(<) -o $(@)

static-fib-c: libfib.o
	gcc -g callfib.c libfib.o -o $(@)

dynamic-fib-c: libfib.so
	gcc -g -L. -lfib callfib.c -o $(@)

static-fib-hs: libfib.o
	ghc -g CallFib.hs libfib.o -o static-fib-hs

dynamic-fib-hs: libfib.so
	ghc -g -no-pie -fPIC -L. -lfib CallFib.hs -o dynamic-fib-hs

overlay.o: overlay.c
	gcc -g -c -finstrument-functions -fpic $(<) -o $(@)

libfib-overlay.so: overlay.o
	gcc -shared -ldl -o $(@) $(<)

.PHONY: clean
clean:
	-rm -f *.o
	-rm -f *.hi
	-rm -f *.so
	-rm -f dynamic-fib-c
	-rm -f static-fib-c
	-rm -f dynamic-fib-hs
	-rm -f static-fib-hs
