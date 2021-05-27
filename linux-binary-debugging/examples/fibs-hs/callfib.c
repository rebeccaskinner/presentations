#include <stdio.h>
#include <stdint.h>

extern uint64_t fib(uint64_t);
extern uint64_t fibsum(uint64_t,uint64_t);

int main() {
  uint64_t fib1 = 9;
  uint64_t fib2 = 11;
  uint64_t a = fib(fib1);
  printf("fib(%lu) = %lu\n", fib1, a);
  uint64_t b = fib(fib2);
  printf("fib(%lu) = %lu\n", fib2, b);
  uint64_t s = fibsum(fib1,fib2);
  printf("%lu + %lu = %lu\n", a, b, s);
  return 0;
}
