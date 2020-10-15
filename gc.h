#include <stdlib.h>
#include <stddef.h>

/*
  Arguments:
  
    - stack_bottom: A pointer to the bottom of the stack (highest address). In
      our runtime, this is set to an address right before entering our_code_starts_here.
    - first_frame: A pointer to the topmost (lowest address) base pointer, for
      starting stack traversal.
    - stack_top: A pointer to the top of the stack (lowest address). During
      execution, this will be the value of RSP - si + 1, so it points to the
      most recent frame's upper-most (lowest-address) variable.
    - heap_start: A pointer to the start of the heap; typically the global
      value HEAP_START.
    - alloc_ptr: The current value of R15 (where the next value would be
      allocated without GC)
  
  Returns:

    The address immediately following the compacted data, to use as the new allocation
    index stored in R15.

*/
int64_t* gc(int64_t* stack_bottom, int64_t* first_frame, int64_t* stack_top, int64_t* heap_start, int64_t* heap_end, int64_t* alloc_ptr);
