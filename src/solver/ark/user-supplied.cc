/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "solver/ark/user-supplied.h"

#include <cstdio>
#include <iostream>

#include "solver/ark/ark.h"
#include "solver/ark/mass.h"
#include "solver/ark/rhs.h"

using namespace flint::solver::ark;

using std::cerr;
using std::endl;

int ArkRhs(realtype t, N_Vector y, N_Vector ydot, void *user_data)
{
	try {
		Ark *ark = static_cast<Ark *>(user_data);
		ark->ReadData(t, y);
		if (!ark->rhs()->Evaluate(ark->data()))
			return -1;
		ark->WriteData(1, ydot);
	} catch (...) {
		cerr << "exception occurred in " << __func__ << endl;
		return -1;
	}
	return 0;
}

int ArkDlsDenseMass(long int /*N*/, realtype t,
					/*N_Vector y,*/ DlsMat M, void *user_data,
					N_Vector /*tmp1*/, N_Vector /*tmp2*/, N_Vector /*tmp3*/)
{
	// Note that M is initialized to the zero matrix
	try {
		Ark *ark = static_cast<Ark *>(user_data);
		ark->ReadTime(t);
		if (!ark->mass()->Evaluate(ark->data()))
			return -1;
		ark->mass()->Write(ark->data(), M);
	} catch (...) {
		cerr << "exception occurred in " << __func__ << endl;
		return -1;
	}
	return 0;
}
