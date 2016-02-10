/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_MATRIX_H_
#define FLINT_RUNTIME_MATRIX_H_

namespace flint {
namespace runtime {

void Transpose(double *d0, const double *d1, int kr, int kc);

void Outerproduct(double *d0,
				  int k1, const double *d1,
				  int k2, const double *d2);

double Scalarproduct(int k, const double *d1, const double *d2);

void Vectorproduct(double *d0, const double *d1, const double *d2);

double Determinant(int k, const double *d1);

/*
 * Note that n2 is of base 1.
 */
double Select2(const double *d1, int n2);

/*
 * Note that n2 and n3 are of base 1.
 */
double Select3(int kr, int kc, const double *d1, int n2, int n3);

/*
 * Note that n2 is of base 1.
 */
void Selrow(double *d0, int kr, int kc, const double *d1, int n2);

void Mult(double *d0, int k, double a1, const double *d2);

void Mmul(double *d0, int kr, int kx, int kc,
		  const double *d1, const double *d2);

}
}

#endif
