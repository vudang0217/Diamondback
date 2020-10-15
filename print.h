#include <stdio.h>
#include <stdlib.h>

/*
  Print the contents of the heap.

  Arguments:

    - heap_start: A pointer to the start of the heap; typically the global
      value HEAP_START.
    - heap_end: A pointer to the last 8-byte word on the heap to be printed.
*/
void print_heap(int64_t* heap_start, int64_t* heap_end);


/*
  Print the contents of the stack.

  Arguments:

    - stack_top: A pointer to the topmost (lowest-address) 8-byte word on the stacsk to be
      printed.
    - stack_bottom: A pointer to the start of the stack.
*/
void print_stack(int64_t* stack_top, int64_t* stack_bottom);
