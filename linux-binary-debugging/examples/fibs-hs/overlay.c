#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <dlfcn.h>

#define FORMAT_OPTIONAL_STRING(X) ((NULL == (X) ) ? ("<?>") : (X))

static void* libfib_handle = NULL;
static uint64_t (*fib_func)(uint64_t) = NULL;
static uint64_t (*fibsum_func)(uint64_t,uint64_t) = NULL;
static void init();

void __cyg_profile_func_enter(void*, void*) __attribute__((no_instrument_function));
void __cyg_profile_func_exit(void*,void*) __attribute__((no_instrument_function));
uint64_t slow_fib(uint64_t);
uint64_t slow_fibsum(uint64_t,uint64_t);
uint64_t fib(uint64_t);
uint64_t fibsum(uint64_t, uint64_t);

__attribute__((constructor(101)))
static void init() {
  if(NULL != fib_func && NULL != fibsum_func) { return ; }
  if(NULL == (libfib_handle = dlopen("libfib.so", RTLD_LAZY))) {
    fprintf(stderr, "Failed to open 'libfib.so', falling back to built-in implementations\n");
    fib_func = slow_fib;
    fibsum_func = slow_fibsum;
    return;
  }
  if (NULL == (fib_func = dlsym(libfib_handle, "fib"))) {
    fprintf(stderr, "Failed to fetch symbol 'fib' from 'libfib.so', falling back to built-in implementation\n");
    fib_func = slow_fib;
  }
  if (NULL == (fibsum_func = dlsym(libfib_handle, "fibsum"))) {
    fprintf(stderr, "Failed to fetch symbol 'fib' from 'libfib.so', falling back to built-in implementation\n");
    fibsum_func = slow_fibsum;
  }
}

void __cyg_profile_func_enter(void* func_start, void* callsite) {
  Dl_info this_function_info;
  Dl_info caller_function_info;
  if((0 == dladdr(func_start, &this_function_info)) || (0 ==dladdr(callsite, &caller_function_info))) {
    fprintf(stderr, "error getting instrumentation information");
    return;
  }
  fprintf(stderr,
          "BEGIN: %s:%s -> %s:%s\n",
          FORMAT_OPTIONAL_STRING(this_function_info.dli_fname),
          FORMAT_OPTIONAL_STRING(this_function_info.dli_sname),
          FORMAT_OPTIONAL_STRING(caller_function_info.dli_fname),
          FORMAT_OPTIONAL_STRING(caller_function_info.dli_sname));
}

void __cyg_profile_func_exit(void* func_start, void* callsite) {
  Dl_info this_function_info;
  Dl_info caller_function_info;
  if((0 == dladdr(func_start, &this_function_info)) || (0 ==dladdr(callsite, &caller_function_info))) {
    fprintf(stderr, "error getting instrumentation information");
    return;
  }
  fprintf(stderr,
          "END: %s:%s -> %s:%s\n",
          FORMAT_OPTIONAL_STRING(this_function_info.dli_fname),
          FORMAT_OPTIONAL_STRING(this_function_info.dli_sname),
          FORMAT_OPTIONAL_STRING(caller_function_info.dli_fname),
          FORMAT_OPTIONAL_STRING(caller_function_info.dli_sname));
}

uint64_t slow_fib(uint64_t n) {
  uint64_t this_fib = 1;
  uint64_t next_fib = 0;
  uint64_t swap = 0;
  while(n-->0) {
    swap = this_fib;
    this_fib = next_fib;
    next_fib += swap;
  }
  return this_fib;
}

uint64_t slow_fibsum(uint64_t a, uint64_t b) {
  return fib_func(a) + fib_func(b);
}


uint64_t fib(uint64_t n) {
  printf("fib says to launch %lu missiles!\n", n);
  return fib_func(n);
}


uint64_t fibsum(uint64_t a, uint64_t b) {
  printf("fibsum says to launch %lu + %lu = %lu missiles!\n", a, b, a + b);
  return fibsum_func(a,b);
}
