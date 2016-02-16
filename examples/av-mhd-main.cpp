#include <stdio.h>
#include <mpi.h>
#include "av-mhd.h"

const int T_MAX = 500;


Formura_Navigator navi;

int init() {
  printf("init\n");
  printf("init %d-%d\n",navi.lower_x, navi.upper_x);

  /*
  for(int x = navi.lower_x; x < navi.upper_x/2; ++x) {
    dens[x] = 0;
    vx[x] = 0.5;
  }
  for(int x = navi.upper_x/2; x < navi.upper_x; ++x) {
    dens[x] = 0;
    vx[x] = -0.25;
    }*/
  for(int x = 90; x < 110; ++x) {
    s[x] = 0.77;
  }
}

int main (int argc, char **argv) {
  MPI_Init(&argc, &argv);
  Formura_Init(&navi, MPI_COMM_WORLD);
  init();
  while(navi.time_step < T_MAX) {
    printf("t = %d\n", navi.time_step);
    {
      char fn[256];
      sprintf(fn, "frames/av-%06d.txt", navi.time_step);
      FILE *fp = fopen(fn,"w");
      for(int x = navi.lower_x; x < navi.upper_x; ++x) {
        fprintf(fp, "%d %f %f %f\n", x, dens[x], vx[x], s[x]);
      }
      fclose(fp);
    }
    Formura_Forward(&navi);
  }
  MPI_Finalize();
}
