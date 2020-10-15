#include "print.h"

#define TRUE  0x0000000000000006L
#define FALSE 0x0000000000000002L

void print_heap_help(int64_t val) {
  if (val == TRUE) {
    printf("true\n");
  } else if (val == FALSE) {
    printf("false\n");
  } else if ((val & 1L) == 1) {
    printf("%ld\n", val >> 1);
  } else if (val == 0) { // null
    printf("null\n");
  } else if ((val & 7L) == 0) { // 7 has 111 at the end   
    printf("%p\n", (int64_t*)val);
  } else {
    printf("Unknown value: %#010lx\n", val);
  }
}

void print_heap(int64_t* heap_start, int64_t* heap_end) {
  //printf("(print_heap) Not yet implemented.\n");
  int64_t* curr = heap_start;

  printf("HEAP_START------------------------------------\n");
  printf("----------------------------------------------\n");
  
  while(curr < heap_end){
    int sz = *(curr + 1);
    printf("%p | GC   - %p\n" , curr, *(curr++));
    printf("%p | Size - %ld\n" , curr, *(curr++));
    printf("%p | Name - %s\n" , curr, *(curr++));
    for(int i = 0; i < sz; i++){
      printf("%p | [%d]  - ", curr, i);
      print_heap_help(*(curr++));
      //printf("\n");
    }
    printf("----------------------------------------------\n");
 }
 printf("HEAP_END--------------------------------------\n");
}

void print_stack(int64_t* stack_top, int64_t* stack_bottom) {
  int64_t* curr = stack_top; 

  printf("STACK_END-------------------------------------\n");
  printf("----------------------------------------------\n");
  
  while(curr < stack_bottom){
    printf("%p | ", curr);
    print_heap_help(*(curr++));
    printf("\n");
  }
  printf("%p | RSP VALUE \n", curr);
  printf("----------------------------------------------\n");
  printf("STACK_START-----------------------------------\n");

}
