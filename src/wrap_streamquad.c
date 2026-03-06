#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_traverse_down_f32_add_mul(float *accR,      // output
                                    float *fractionR, // input
                                    int *sourceR,     // ptrdiff_t
                                    int *targetR,     // ptrdiff_t
                                    int *edge_countR  // ptrdiff_t
                                    ) {

  // Transformation of integers and array allocation
  ptrdiff_t edge_count = edge_countR[0];
  ptrdiff_t *source = R_Calloc(edge_count, ptrdiff_t);
  ptrdiff_t *target = R_Calloc(edge_count, ptrdiff_t);
  
  // Convert sourceR and targetR to ptrdiff_t
  for (ptrdiff_t idx = 0; idx < edge_count; idx++) {
    source[idx] = (ptrdiff_t)sourceR[idx];
    target[idx] = (ptrdiff_t)targetR[idx];
  }

  // Flow accumulation computation using libtopotoolbox
  traverse_down_f32_add_mul(accR, fractionR, source, target, edge_count);
  
  // Free memory
  R_Free(source);
  R_Free(target);
}
