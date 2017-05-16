/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "solver.h"

#include "job.h"
#include "task.h"
#include "solver/ark.h"

namespace flint {
namespace solver {

bool Solve(sqlite3 *db, Method method, task::Task &task, const job::Option &option)
{
	switch (method) {
	case Method::kArk:
		return ark::Solve(db, task, option);
	}
	// TODO
	return false;
}

}
}
