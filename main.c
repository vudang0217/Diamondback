#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "gc.h"
#include "print.h"

#define TRUE  0x0000000000000006L
#define FALSE 0x0000000000000002L

#define BOA_MIN (- (1L << 62))
#define BOA_MAX ((1L << 62) - 1)

extern int64_t our_code_starts_here(int64_t* THE_HEAP, int64_t input_val) asm("our_code_starts_here");
extern int64_t print(int64_t input_val) asm("print");
extern void    error(int64_t val) asm("error");
extern int64_t equal(int64_t val1, int64_t val2) asm("equal");

extern int64_t* HEAP_END asm("HEAP_END");
extern int64_t* STACK_BOTTOM asm("STACK_BOTTOM");
extern int64_t* FINAL_HEAP_PTR asm("FINAL_HEAP_PTR");

extern int64_t* try_gc(int64_t* alloc_ptr,
                       int64_t words_needed,
                       int64_t* first_frame,
                       int64_t* stack_top) asm("try_gc");

int64_t* HEAP_START;     // This is set in setup_heap
int64_t* HEAP_END;       // This is set in setup_heap
int64_t* FINAL_HEAP_PTR; // This is set by the program assembly
int64_t* STACK_BOTTOM;   // This is set by the program assembly

int64_t  DEFAULT_HEAP_SIZE = 10000;

typedef struct {
  int64_t gc_metadata;
  int64_t size;
  char* name;
  int64_t elements[];
} Data;

int64_t equal(int64_t val1, int64_t val2) {
  if(val1 == val2) { return TRUE; }
  else if((val1 & val2 & 7L) == 0) {
    Data* d1 = (Data*) val1;
    Data* d2 = (Data*) val2;
    if(d1->name != d2->name) {
      return FALSE;
    }
    else {
      for(int i = 0; i < d1->size; i += 1) {
        if(equal(d1->elements[i], d2->elements[i]) == FALSE) { return FALSE; }
      }
      return TRUE;
    }
  }
  else {
    return FALSE;
  }
}

int64_t print_rec(int64_t val) {
  if (val == TRUE) {
    printf("true");
  } else if (val == FALSE) {
    printf("false");
  } else if ((val & 1L) == 1) {
    printf("%ld", val >> 1);
  } else if (val == 0) { // null
    printf("null");
  } else if ((val & 7L) == 0) { // 7 has 111 at the end
    Data* d = (Data*) val;
    printf("(%s ", d->name);
    for(int i = 0; i < d->size; i += 1) {
      print_rec(d->elements[i]);
      if(i < d->size - 1) { printf(" "); }
    }
    printf(")");
  } else {
    printf("Unknown value: %#010lx", val);
  }
  return val;
}
int64_t print(int64_t val) {
  print_rec(val);
  printf("\n");
  return val;
}

void error(int64_t error_code) {
  if(error_code == 1)
    fprintf(stderr, "Error: expected a number\n");
  else if (error_code == 2)
    fprintf(stderr, "Error: expected a boolean\n");
  else if (error_code == 3)
    fprintf(stderr, "Error: overflow\n");
  else if (error_code == 4)
    fprintf(stderr, "Error: input is not a representable number\n");
  else if (error_code == 5)
    fprintf(stderr, "Error: input must be a number\n");
  else if (error_code == 6)
    fprintf(stderr, "Error: null pointer exception\n");
  else if (error_code == 7)
    fprintf(stderr, "Error: heap size is not a valid number\n");

  exit(123456);
}

/*
  STUDENT: This function sets up the program's heap by setting the following
  global variables:

  HEAP_START is passed as an argument to our_code_starts_here, and should
             point to the first address the program can store heap-allocated
             data at.
  HEAP_END   is accessed directly by our_code_starts_here, and used to
             determine when to call try_gc(). It should point to the
             address AFTER the last word on the heap.

  You may want to change this function depending on what GC algorithm you
  choose to implement.

  Arguments:

  - heap_size: The heap size requested for the program. This is read in
    main() below.
*/
void setup_heap(int64_t heap_size) {
  HEAP_START = calloc(heap_size, sizeof(int64_t));
  HEAP_END   = HEAP_START + heap_size;
}

/*
  STUDENT: You do not need to change this main function, but it's worth reading!

  It does three things that are interesting relative to Diamondback:

  1. It uses the first command-line argument as the initial size of the heap
     (in words)
  2. It initializes the heap by calling setup_heap(), setting global variables
     that are shared with our_code_starts_here and try_gc functions.
  3. It uses an optional third command-line argument to determine if it should
     print the heap content at the end of the program's execution.

  Other than that, it's just an implementation of what you had to do in PA5,
  but shifted by one argument (since the first argument now means heap size)
*/
int main(int argc, char** argv) {
  char * endptr;
  extern int errno;

  int64_t input_val = 1;
  int64_t dump_heap = 0;
  int64_t heap_size = DEFAULT_HEAP_SIZE;

  if (argc > 1) {
    // Read heap size
    endptr = (char*) &argv[1];
    errno = 0;
    int64_t r = strtol(argv[1], &endptr, 10);

    if (*endptr != '\0' || errno || r < 0) error(7);

    heap_size = r;
  }

  if (argc > 2) {
    // Read input
    endptr = (char*) &argv[2];
    errno = 0;
    int64_t r = strtol(argv[2], &endptr, 10);

    if (*endptr != '\0') error(5);
    else if (errno || r < BOA_MIN || r > BOA_MAX) error(4);

    input_val = r << 1 | 1;
  }

  if (argc > 3) {
    // Read dump heap argument
    const char* arg = (char*) argv[3];

    if (strcmp(arg, "dump") == 0) {
      dump_heap = 1;
    }
  }

  setup_heap(heap_size);
  int64_t result = our_code_starts_here(HEAP_START, input_val);
  print(result);

  if (dump_heap) {
      print_heap(HEAP_START, FINAL_HEAP_PTR);
  }

  free(HEAP_START);

  return 0;
}


/*
  Try to clean up space in memory by calling gc.

  Arguments:

  - alloc_ptr: The current value of R15 (where the next value would be
    allocated without GC)
  - words_needed: The number of _words_ that the runtime is trying to allocate
  - first_frame: The current value of RSP
  - stack_top: The address of the highest value on the stack that is still in use.

  Returns:

  The new value for R15, for the runtime to start using as the allocation
  point. Must be set to a location that provides enough room to fit
  words_needed, and more words in the given heap space if possible.
*/
int64_t* try_gc(int64_t* alloc_ptr,
                int64_t  words_needed,
                int64_t* first_frame,
                int64_t* stack_top) {

  if(HEAP_START == alloc_ptr) {
    fprintf(stderr, "Allocation of %ld words too large for %ld-word heap\n", words_needed, HEAP_END - HEAP_START);
    free(HEAP_START);
    exit(10);
  }

  int64_t* new_r15 = gc(STACK_BOTTOM, first_frame, stack_top, HEAP_START, HEAP_END, alloc_ptr);

  if((new_r15 + words_needed) > HEAP_END) {
    fprintf(stderr, "Out of memory: needed %ld words, but only %ld remain after collection\n", words_needed, (HEAP_END - new_r15));
    free(HEAP_START);
    exit(9);
  }

  return new_r15;
}
