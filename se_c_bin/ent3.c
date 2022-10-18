#include <stdio.h>
#include <stdlib.h>

#define N 8

typedef struct cfloat_ {
	float real;
	float imag;
} cfloat;

cfloat cadd(cfloat a, cfloat b) {
	return (cfloat) {a.real + b.real, a.imag + b.imag};
}

cfloat cmul(cfloat a, cfloat b) {
	return (cfloat) {a.real * b.real - a.imag * b.imag, a.real * b.imag + a.imag * b.real};
}

cfloat cdot (cfloat a0, cfloat a1, cfloat b0, cfloat b1) {
	return cadd(cmul(a0, b0), cmul(a1, b1));
}

cfloat cneg(cfloat a) {
	return (cfloat) {-a.real, -a.imag};
}

#define M_SQRT1_2 (cfloat) {0.707106781186547524400844362104849039, 0}

int main(int argc, char **argv) {
	cfloat *state = (cfloat *) malloc(N * sizeof(cfloat));
	
	// Initialize state
	for (int i = 0; i < N; i++) {
		state[i] = (cfloat) {0.0, 0.0};
	}
	state[0] = (cfloat) {1.0, 0.0};

	// Allocate output state buffer
	cfloat *out_state = (cfloat *) malloc(N * sizeof(cfloat));
	
	out_state[0] = cadd(cmul(M_SQRT1_2,state[0]),cmul(M_SQRT1_2,state[1]));
	out_state[1] = cadd(cmul(M_SQRT1_2,state[6]),cmul(cneg(M_SQRT1_2),state[7]));
	out_state[2] = cadd(cmul(M_SQRT1_2,state[2]),cmul(M_SQRT1_2,state[3]));
	out_state[3] = cadd(cmul(M_SQRT1_2,state[4]),cmul(cneg(M_SQRT1_2),state[5]));
	out_state[4] = cadd(cmul(M_SQRT1_2,state[4]),cmul(M_SQRT1_2,state[5]));
	out_state[5] = cadd(cmul(M_SQRT1_2,state[2]),cmul(cneg(M_SQRT1_2),state[3]));
	out_state[6] = cadd(cmul(M_SQRT1_2,state[6]),cmul(M_SQRT1_2,state[7]));
	out_state[7] = cadd(cmul(M_SQRT1_2,state[0]),cmul(cneg(M_SQRT1_2),state[1]));
	

	// Print state
	for (int i = 0; i < N; i++) {
		printf("|%d>: %f + %fi\n", i, out_state[i].real, out_state[i].imag);
	}	

	return 0;
}