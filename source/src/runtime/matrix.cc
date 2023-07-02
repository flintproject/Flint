/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "runtime/matrix.h"

#include <cassert>

#include <Eigen/Dense>
#include <Eigen/Geometry>
#include <Eigen/LU>

using Eigen::Map;
using Eigen::Matrix3d;
using Eigen::MatrixXd;
using Eigen::Vector3d;
using Eigen::VectorXd;

namespace flint {
namespace runtime {

void Transpose(double *d0, const double *d1, int kr, int kc)
{
	Map<MatrixXd> m0(d0, kr, kc);
	Map<const MatrixXd> m1(d1, kc, kr);
	m0 = m1.transpose();
}

void Outerproduct(double *d0,
				  int k1, const double *d1,
				  int k2, const double *d2)
{
	Map<const VectorXd> v1(d1, k1);
	Map<const VectorXd> v2(d2, k2);
	Map<MatrixXd> m0(d0, k2, k1);
	m0.noalias() = v1 * v2.transpose();
}

double Scalarproduct(int k, const double *d1, const double *d2)
{
	Map<const VectorXd> v1(d1, k);
	Map<const VectorXd> v2(d2, k);
	return v1.dot(v2);
}

void Vectorproduct(double *d0, const double *d1, const double *d2)
{
	Map<Vector3d> v0(d0);
	Map<const Vector3d> v1(d1);
	Map<const Vector3d> v2(d2);
	v0 = v1.cross(v2);
}

double Determinant(int k, const double *d1)
{
	Map<const MatrixXd> m1(d1, k, k);
	return m1.determinant();
}

double Select2(const double *d1, int n2)
{
	assert(n2 > 0); // 1-based
	return d1[n2 - 1];
}

double Select3(int kr, int kc, const double *d1, int n2, int n3)
{
	assert(n2 > 0);
	assert(n3 > 0);
	Map<const MatrixXd> m1(d1, kr, kc);
	return m1(n2 - 1, n3 - 1);
}

void Selrow(double *d0, int kr, int kc, const double *d1, int n2)
{
	assert(n2 > 0); // 1-based
	Map<VectorXd> v0(d0, kr);
	Map<const MatrixXd> m1(d1, kr, kc);
	v0 = m1.row(n2 - 1);
}

void Mult(double *d0, int k, double a1, const double *d2)
{
	Map<VectorXd> v0(d0, k);
	Map<const VectorXd> v2(d2, k);
	v0.noalias() = a1 * v2;
}

void Mmul(double *d0, int kr, int kx, int kc,
		  const double *d1, const double *d2)
{
	Map<MatrixXd> m0(d0, kr, kc);
	Map<const MatrixXd> m1(d1, kr, kx);
	Map<const MatrixXd> m2(d2, kx, kc);
	m0.noalias() = m1 * m2;
}

}
}
