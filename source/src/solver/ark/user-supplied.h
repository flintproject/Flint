/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_ARK_USER_SUPPLIED_H_
#define FLINT_SOLVER_ARK_USER_SUPPLIED_H_

#include <arkode/arkode.h>
#include <arkode/arkode_dense.h>
#include <arkode/arkode_direct.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Of type ARKRhsFn.
 */
int ArkRhs(realtype t, N_Vector y, N_Vector ydot, void *user_data);

/*
 * Of type ARKDlsDenseMassFn.
 */
int ArkDlsDenseMass(long int N, realtype t,
					/*N_Vector y,*/ DlsMat M, void *user_data,
					N_Vector tmp1, N_Vector tmp2, N_Vector tmp3);

#ifdef __cplusplus
}
#endif

#endif
