/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_ARK_H_
#define FLINT_SOLVER_ARK_H_

namespace flint {

namespace job {
struct Option;
}

namespace task {
struct Task;
}

namespace solver {

namespace ark {

bool Solve(task::Task &task, const job::Option &option);

}
}
}

#endif
