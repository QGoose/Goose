#include <stdio.h>
#include <stdlib.h>

#define N 16

typedef struct cfloat_ {
  float real;
  float imag;
} cfloat;

cfloat cadd(cfloat a, cfloat b) {
  return (cfloat){a.real + b.real, a.imag + b.imag};
}

cfloat csub(cfloat a, cfloat b) {
  return (cfloat){a.real - b.real, a.imag - b.imag};
}

cfloat cmul(cfloat a, cfloat b) {
  return (cfloat){a.real * b.real - a.imag * b.imag,
                  a.real * b.imag + a.imag * b.real};
}

cfloat cdot(cfloat a0, cfloat a1, cfloat b0, cfloat b1) {
  return cadd(cmul(a0, b0), cmul(a1, b1));
}

cfloat cneg(cfloat a) { return (cfloat){-a.real, -a.imag}; }

#define SQRT1_2                                                                \
  (cfloat) { 0.707106781186547524400844362104849039, 0 }

// {{ExtraConstants}}

int main(int argc, char **argv) {
  cfloat *state = (cfloat *)malloc(N * sizeof(cfloat));

  // Initialize state
  for (int i = 0; i < N; i++) {
    state[i] = (cfloat){0.0, 0.0};
  }
  state[0] = (cfloat){1.0, 0.0};

  // Allocate output state buffer
  cfloat *out_state = (cfloat *)malloc(N * sizeof(cfloat));

  out_state[0] = cadd(cmul(SQRT1_2, state[0]), cmul(SQRT1_2, state[1]));
  out_state[1] = cadd(cmul(SQRT1_2, state[14]), cmul(cneg(SQRT1_2), state[15]));
  out_state[2] = cadd(cmul(SQRT1_2, state[2]), cmul(SQRT1_2, state[3]));
  out_state[3] = cadd(cmul(SQRT1_2, state[12]), cmul(cneg(SQRT1_2), state[13]));
  out_state[4] = cadd(cmul(SQRT1_2, state[4]), cmul(SQRT1_2, state[5]));
  out_state[5] = cadd(cmul(SQRT1_2, state[10]), cmul(cneg(SQRT1_2), state[11]));
  out_state[6] = cadd(cmul(SQRT1_2, state[6]), cmul(SQRT1_2, state[7]));
  out_state[7] = cadd(cmul(SQRT1_2, state[8]), cmul(cneg(SQRT1_2), state[9]));
  out_state[8] = cadd(cmul(SQRT1_2, state[8]), cmul(SQRT1_2, state[9]));
  out_state[9] = cadd(cmul(SQRT1_2, state[6]), cmul(cneg(SQRT1_2), state[7]));
  out_state[10] = cadd(cmul(SQRT1_2, state[10]), cmul(SQRT1_2, state[11]));
  out_state[11] = cadd(cmul(SQRT1_2, state[4]), cmul(cneg(SQRT1_2), state[5]));
  out_state[12] = cadd(cmul(SQRT1_2, state[12]), cmul(SQRT1_2, state[13]));
  out_state[13] = cadd(cmul(SQRT1_2, state[2]), cmul(cneg(SQRT1_2), state[3]));
  out_state[14] = cadd(cmul(SQRT1_2, state[14]), cmul(SQRT1_2, state[15]));
  out_state[15] = cadd(cmul(SQRT1_2, state[0]), cmul(cneg(SQRT1_2), state[1]));

  // Print state
  for (int i = 0; i < N; i++) {
    printf("|%d>: %f + %fi\n", i, out_state[i].real, out_state[i].imag);
  }

  return 0;
}
