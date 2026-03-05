#include <R.h>
#include <stddef.h>
#include <stdint.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_excesstopography(float *excess, float *dem, float *threshold_slopes,
                           float *cellsize, int *dimsR) {
  // Transformation of integers and array allocation
  ptrdiff_t dims[2] = {dimsR[0], dimsR[1]};

  // Call libtopotoolbox
  excesstopography_fsm2d(excess, dem, threshold_slopes, *cellsize, dims);
}
