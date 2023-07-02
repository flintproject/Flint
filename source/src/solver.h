/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_H_
#define FLINT_SOLVER_H_

#include <cstdio>

#include "job.h"

namespace flint {

namespace task {
struct Task;
}

namespace solver {

enum class Method {
	kArk
};

job::Result Solve(Method method, task::Task &task, const job::Option &option);

}
}

#endif
