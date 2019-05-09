#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

typedef struct {
  int _age;
  char _name[];
} person_t;

person_t* make_person(int age, char* name __attribute__((nonnull))) {
  size_t name_len = strlen(name);
  size_t sz = sizeof(int) + name_len + 1;
  person_t* m = malloc(sz);
  memset(m, 0, sz);
  m->_age = age;
  memcpy(m + sizeof(int), name, name_len);
  return m;
}

#define id(x) x;

int main() {
  int age = id(35);
  person_t* p = id(make_person(age, "rebecca"));
  printf("Happy %d, %s!\n", p->_age, p->_name);
}
