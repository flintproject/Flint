/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "solver.h"

#include "task.h"
#include "solver/ark.h"

namespace flint {
namespace solver {

job::Result Solve(Method method, task::Task &task, const job::Option &option)
{
	switch (method) {
	case Method::kArk:
		return ark::Solve(task, option);
	}
	// TODO
	return job::Result::kFailed;
}

}
}
