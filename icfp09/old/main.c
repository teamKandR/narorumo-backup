#include <stdio.h>
#include <stdlib.h>

int topfour(int code) {
  int masked = code & 0xf0000000;

  return (masked >> (32 - 4));
}

int nextfour(int code) {
  int masked = code & 0x0f000000;

  return (masked >> (32 - 8));
}

int main(int argc, char *argv[]) {

  if (argc < 2) {
    printf("usage: %s fn\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  FILE *fn = fopen(argv[1], "r");

  char frame[13]; // 12 byte frames.
  int odd = 0;

  double data;
  double *datap;

  int opcode;
  int *opcodep;

  while (NULL != fgets(frame, 13, fn)) {
    opcodep = (int *) (frame + (odd? 0 : 8));
    opcode = *opcodep;

    datap = (double*) (frame + (odd? 4 : 0) );
    data = *datap;

    printf("operation: %x, data value: %f\n", opcode, data);
    // printf("top four bits: 0x%x, ", topfour(opcode));
    // printf("next four bits: 0x%x\n", nextfour(opcode));

    odd = !odd;  
  }

  exit(EXIT_SUCCESS);
}
