.PHONY: all
all: fibs-stripped   \
     fibs-unstripped

LIBS= -lc

%.o: %.s
	nasm -felf64 $(<) -o $(@)

fibs-stripped: fibs.o
	gcc $(<) $(LIBS) -o $(@)
	strip $(@)

fibs-unstripped: fibs.o
	gcc $(<) $(LIBS) -o $(@)

.PHONY: clean
clean:
	-rm -fr *.o
	-rm -fr fibs-{stripped,unstripped}
