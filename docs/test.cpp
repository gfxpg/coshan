#include <hip/hip_runtime.h>

__global__ void cond_write(float *C, const float *A, float B) {
  if (*A == B) {
    *C = 0x666;
  }
}

__global__ void simple_for_loop(float *C, size_t N) {
  for (size_t i = 0; i < N; ++i) C[i] += 9;
}
