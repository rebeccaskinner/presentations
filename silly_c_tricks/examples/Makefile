CC=gcc
CFLAGS= -std=gnu99 \
	-Wall \
	-Werr \
	-Wextra

all: index closure

%.o: %.c
	$(CC) -c $(<) -o $(@)

index: index.o
	gcc $(<) -o $(@)

closure: closure.o
	gcc $(<) -o $(@)

curry: curry.o
	gcc $(<) -o $(@)

clean:
	rm -fr *.o
	rm -fr index
	rm -fr closure
	rm -fr curry

.PHONY: all clean
