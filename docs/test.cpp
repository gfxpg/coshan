#include <hip/hip_runtime.h>

__global__ void cond_write(float *C, const float *A, float B) {
  if (*A == B) {
    *C = 0x666;
  }
}

__global__ void loop_with_cond(float* C, size_t N)
{
    while (1)
    {
        while (1)
        {
            C[0] += 1;
            if (C[0] > 1)
                break;
            C[1] += 5;
        }
        C[2] = 0;
    }
}
