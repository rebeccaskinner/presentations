.PHONY: all
all: min-dynamic-stripped min-dynamic-unstripped min-static-stripped min-static-unstripped

%.o: %.s
	nasm -felf64 $(<) -o $(@)

min-static-stripped: min.o
	ld -static $(<) -o $(@)
	strip $(@)

min-dynamic-stripped: min.o
	ld $(<) -o $(@)
	strip $(@)

min-static-unstripped: min.o
	ld -static $(<) -o $(@)

min-dynamic-unstripped: min.o
	ld $(<) -o $(@)

.PHONY: clean
clean:
	-rm -fr *.o
	-rm -fr min-{static,dynamic}-{stripped,unstripped}
