/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_ARK_H_
#define FLINT_SOLVER_ARK_H_

#include "job.h"

namespace flint {

namespace task {
struct Task;
}

namespace solver {

namespace ark {

job::Result Solve(task::Task &task, const job::Option &option);

}
}
}

#endif
