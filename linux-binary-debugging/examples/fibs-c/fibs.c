#include <stdio.h>

void fibs(unsigned int count) {
  unsigned long current_fib = 0;
  unsigned long next_fib = 1;
  unsigned long scratch = 0;
loop:
  printf("%20ld\n", current_fib);
  count = count - 1;
  if(count == 0) {
    goto done;
  }
  scratch = current_fib;
  current_fib = next_fib;
  next_fib = next_fib + scratch;
  goto loop;
done:
  return;
}


int main() {
  fibs(90);
  return 0;
}
