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

cfloat csub(cfloat a, cfloat b) {
	return (cfloat) {a.real - b.real, a.imag - b.imag};
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

#define SQRT1_2 (cfloat) {0.707106781186547524400844362104849039, 0}

// {{ExtraConstants}}

int main(int argc, char **argv) {
	cfloat *state = (cfloat *) malloc(N * sizeof(cfloat));
	
	// Initialize state
	for (int i = 0; i < N; i++) {
		state[i] = (cfloat) {0.0, 0.0};
	}
	state[0] = (cfloat) {1.0, 0.0};

	// Allocate output state buffer
	cfloat *out_state = (cfloat *) malloc(N * sizeof(cfloat));
	
	out_state[0] = cmul(SQRT1_2,cadd(state[0],state[1]));
	out_state[1] = cmul(SQRT1_2,csub(state[6],state[7]));
	out_state[2] = cmul(SQRT1_2,cadd(state[2],state[3]));
	out_state[3] = cmul(SQRT1_2,csub(state[4],state[5]));
	out_state[4] = cmul(SQRT1_2,cadd(state[4],state[5]));
	out_state[5] = cmul(SQRT1_2,csub(state[2],state[3]));
	out_state[6] = cmul(SQRT1_2,cadd(state[6],state[7]));
	out_state[7] = cmul(SQRT1_2,csub(state[0],state[1]));
	

	// Print state
	for (int i = 0; i < N; i++) {
		printf("|%d>: %f + %fi\n", i, out_state[i].real, out_state[i].imag);
	}	

	return 0;
}