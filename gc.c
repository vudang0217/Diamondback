#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "print.h"
#include "gc.h"

// STUDENT: Change this to 1 in order to print
#define DEBUG 0

#define DEBUG_PRINT(...) \
     do { if (DEBUG) fprintf(stdout, __VA_ARGS__); } while (0)

int64_t* MAX_ADDR;
int64_t* MIN_ADDR;

typedef struct {
  int64_t gc_metadata;
  int64_t size;
  char* name;
  int64_t elements[];
} Data;

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

void mark_heap(Data* r){
     if(DEBUG){
          printf("entering mark heap, looking at pointer %p\n", r);
     }
     if(r->gc_metadata==1){
          return;
     }
     r->gc_metadata = 1;
     int64_t curr;
     for(int i = 0; i < r->size; i++){
          curr = (r->elements)[i]; // curr  is the current  element of Data
          if((curr) && ((curr & 0x7L) == 0)){ // if element is an address -- 0x7L = 000...00111  check if it's not null
               mark_heap((Data*)curr);     
          }
     }
     MAX_ADDR = (((MAX_ADDR) > ((int64_t*)r)) ? (MAX_ADDR) : ((int64_t*)r)); // save max address of live data
     MIN_ADDR = (((MIN_ADDR) < ((int64_t*)r)) ? (MIN_ADDR) : ((int64_t*)r)); // save min address of live data
}

Data* forward(Data* r){
     if(DEBUG){
          printf("entering FORWARD heap, looking at pointer %p\n", r);
     }
     if((r->gc_metadata & 0x1L)==1){ // visited /
          Data* unmarked = ((r->gc_metadata) >> 1) << 1;
          if(DEBUG){
               printf("%p VISITED, new address for that is %p\n", r, unmarked);
               
          }
          return unmarked; // return unmarked move_to address 
     }
     int64_t unmarked_addr = r->gc_metadata;
     r->gc_metadata = (r->gc_metadata) | 0x1; // mark the visited Data , change LSB to 1 -and with- 11...0001
     if(DEBUG){
          printf("unmarked gc_metada aka new address on heap after gc is %p\n", unmarked_addr);
          printf("marked gc word is %p\n", r->gc_metadata);
     }

     int64_t curr;
     int sz = r->size;
     for(int i = 0; i < sz; i++){
          curr = (r->elements)[i]; // curr  is the current  element of Data
          if((curr) && ((curr & 0x7L) == 0)){ // if element is an address
              (r->elements)[i] = forward((Data*)curr);
          }
     }
     return unmarked_addr; 

}

int64_t* gc(int64_t* stack_bottom,
            int64_t* first_frame,
            int64_t* stack_top,
            int64_t* heap_start,
            int64_t* heap_end,
            int64_t* alloc_ptr) {
     DEBUG_PRINT("starting GC...\n");
     DEBUG_PRINT("\tstack top    = 0x%p\n\tstack_bottom = 0x%p\n\tfirst_frame  = 0x%p\n\theap start   = 0x%p\n\theap_end     = 0x%p\n",
                 stack_top,
                 stack_bottom,
                 first_frame,
                 heap_start,
                 heap_end);

     // STUDENT: You will change this code (and edit anything you like about debug
     // prints, etc), to collect garbage and finally return the new value for R15
     // to use to start allocation from.
     if(DEBUG){
          printf("BEFORE GARBAGE COLLECTION \n");
          print_stack(stack_top, stack_bottom);
          printf("\n");
          print_heap(heap_start, alloc_ptr);
          printf("\n");
     }

     int64_t* curr = stack_bottom;
     int64_t* move_to; // address to heap
     Data* move_from; // address to heap
     int data_size; 

     MIN_ADDR = heap_end;
     MAX_ADDR = heap_start;

     //mark
     while(curr >= stack_top){
          if((*curr != 0) && ((*curr & 7L) == 0) && (*curr >= heap_start) && (*curr < heap_end)){ //current variable is a reference to heap, check if ref to null
               if(DEBUG){
                    printf("entering STACK, address is %p\n", curr); 
               }
               mark_heap(*curr);
          }
          curr--; 
     } //note that on stack the variable is formated to representation

     
     move_to = heap_start; // replace dead data before the min_address 
     move_from = MIN_ADDR; // smallest address referenced 

     if(DEBUG){
          printf("AFTER MARK \n");
          print_stack(stack_top, stack_bottom);
          printf("\n");
          print_heap(heap_start, alloc_ptr);
          printf("\n");
          printf("max address : %p\n", MAX_ADDR);
          printf("move_from (min address) : %p\n", move_from);

          printf("\n");
          printf("In while loop...\n");
     }

     // forward 1 -- change  gc word to the new spot on heap 
     while(move_from <= MAX_ADDR){
          if(move_from->gc_metadata){  //if marked 
               move_from->gc_metadata = move_to;
               if(DEBUG){
                    printf("current move_to = %p\n", move_to);
                    printf("gc metadata = %p\n", move_from->gc_metadata);
               }
               move_to += (move_from->size + 3);
               if(DEBUG){
                    printf("move_to after add = %p\n", move_to);
               }
          }
          move_from = (int64_t*)move_from + move_from->size +3;
          if(DEBUG){
                    printf("move_from after add = %p\n", move_from);
                    printf("MAX_ADDRR = %p\n", MAX_ADDR);
                    printf("while iteration bool : %d\n", move_from <= MAX_ADDR);
                    printf("\n");
          }
     }
     //if space is already compacted, move_to = alloc_ptr

     if(DEBUG){
          printf("\n");
          printf("AFTER FORWARD 1 \n");
          print_stack(stack_top, stack_bottom);
          printf("\n");
          print_heap(heap_start, alloc_ptr);
          printf("\n");
     }

     // forward 2 
     curr = stack_bottom;
     while(curr >= stack_top){
          //current variable is a reference to heap, check not null
          if((*curr != 0) && ((*curr & 7L) == 0) && (*curr >= heap_start) && (*curr <= heap_end)){ 
               //printf("ref on stack (curr) is %p\n", *curr);
               *curr = forward(*curr); // change address of reference 
          }
          curr--;
     }
     if(DEBUG){
          printf("AFTER FORWARD 2 \n");
          print_stack(stack_top, stack_bottom);
          printf("\n");
          print_heap(heap_start, alloc_ptr);
          printf("\n");
          printf("\n");
          printf("MIN_ADDR and move_from = %p\n", MIN_ADDR);
     }

     // compact 
     move_to = heap_start;
     move_from  = MIN_ADDR;
     if(DEBUG){
          printf("move_from  %p\n", move_from);
          printf("move_to  %p\n", move_to);
          printf("\n");
     }
     while (move_from  <= MAX_ADDR){
          int sz = move_from->size;
          if(move_from->gc_metadata){ // if gc contains an address, meaning live data
               //printf("\n");
               //printf("inside while, move_from = %p\n", move_from);
               //printf("inside while, current move_to = %p\n", move_to);
               *move_to = 0; //move move_from to move_to
               //printf("move_to addr = %p  |  pointing to %p\n", move_to, *move_to);
               move_to++;
               *move_to = move_from ->size; //space is OKAY!
               //printf("move_to addr = %p  |  pointing to %d\n", move_to, *move_to);
               move_to++;
               *move_to = move_from ->name; 
               //printf("move_to addr = %p  |  pointing to %s\n", move_to, *move_to);
               move_to++;
               //move_to[3] = move_from ->elements; //refresh gc word
                // bug here -> as you move up, move_from->size gets overwritten 
               for(int i = 0; i<sz; i++){
                    *move_to = (move_from->elements)[i];
                    //printf("move_to addr = %p  |  pointing to %p\n", move_to, *move_to);
                    move_to++;
               }
               //printf("move_to after if = %p\n", move_to);
               //move_to += (move_from ->size + 3); //number of elements plus 3 for  
          }
          move_from  = (int64_t*)move_from + sz +3;
          //printf("move_from after add = %p\n", move_from);
     }
     alloc_ptr = move_to;

     if(DEBUG){
          printf("AFTER GARBAGE COLLECTION \n");
          print_stack(stack_top, stack_bottom);
          printf("\n");
          print_heap(heap_start, alloc_ptr);
          printf("\n");
          printf("move_to_final = %p\n", move_to);
          printf("alloc_ptr = %p\n", alloc_ptr);
     }
     
     //DEBUG_PRINT("(gc) Not yet implemented.");

     return alloc_ptr;
}
