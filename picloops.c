/* compile with "gcc -opicloops picloops.c -lm" */

#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <stdlib.h>

#define MACHINE_CYCLE_MAX 50464002.0
#define SINGLE_STAGE_MAX 771.0
#define TWO_STAGE_MAX 197125.0
#define THREE_STAGE_MAX MACHINE_CYCLE_MAX

int main(int argc, char **argv) {
  int counterA, counterB, counterC;
  double time;
  double cycles;

  counterA = counterB = counterC = -1;
  time = 4.0 / (atof(argv[2]) * 1000000.0);
  cycles = floor(atof(argv[1]) / time);

  if(cycles <= SINGLE_STAGE_MAX)
    counterA = ((cycles / 3.0) - 1.0);
  else if(cycles <= TWO_STAGE_MAX) {
    counterB = ((cycles + 763.0) / 770.0);
    counterA = ((cycles + (763.0 - (770.0 * counterB))) / 3.0);
  }
  else if(cycles <= THREE_STAGE_MAX) {
    counterC = ((cycles - 3.0) / 197125.0 + 1.0);
    counterB = (((cycles - 3.0) / counterC + 758.0) / 770.0);
    counterA = (((cycles - 3.0) / counterC + 758.0 - 770.0 * counterB) / 3.0);
  }
  if(counterA == 256)
    counterA = 0;
  if(counterB == 256)
    counterB = 0;
  if(counterC == 256)
    counterC = 0;    

  printf("%d %d %d\n", counterA, counterB, counterC);
}

    
