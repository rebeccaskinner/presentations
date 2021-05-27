#include <stdio.h>
#include <stdint.h>

void fibs(uint64_t count) {
  uint64_t current_fib = 0;
  uint64_t next_fib = 1;
  uint64_t scratch = 0;
  while(count--) {
    printf("%20ld\n", current_fib);
    scratch = current_fib;
    current_fib = next_fib;
    next_fib = next_fib + scratch;
  }
}


int main() {
  fibs(90);
  return 0;
}
