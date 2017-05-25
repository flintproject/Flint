/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/job-runner.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "compiler.h"
#include "db/read-only-driver.h"
#include "exec/task-runner.h"
#include "job.h"
#include "runtime.h"
#include "task.h"

namespace flint {
namespace exec {

namespace {
const int kFilenameLength = 96;
}

JobRunner::JobRunner(TaskRunner *tr, int id)
	: tr_(tr)
	, id_(id)
	, dir_(job::BuildPath(tr->dir(), id))
	, generated_db_(new char[kFilenameLength])
	, isd_(new char[kFilenameLength])
{
	std::sprintf(generated_db_.get(), "%s/generated.db", dir_.get());
	std::sprintf(isd_.get(), "%s/out.isd", dir_.get());
}

bool JobRunner::Run()
{
	if (tr_->GetTask()->IsCanceled())
		return true;

	std::vector<double> generated_init;
	{
		std::unique_ptr<Bytecode> generated_bc;
		{
			compiler::Compiler c(tr_->GetDimensionAnalyzer());
			db::ReadOnlyDriver g(generated_db_.get());
			generated_bc.reset(c.Compile(g.db(), "parameter_eqs", compiler::Method::kAssign));
			if (!generated_bc)
				return false;
		}
		// TODO: give a proper seed if desired
		if (!runtime::Eval(tr_->GetDatabase(), ct::Availability::kNone, 0,
						   tr_->generated_layout(), generated_bc.get(), &generated_init))
			return false;
	}
	std::vector<double> init(tr_->data()); // copy data
	if (!job::Store(tr_->GetDatabase(), tr_->generated_layout(), generated_init.data(), tr_->layout(), init.data()))
		return false;
	if (tr_->GetTask()->reinit_bc) {
		// Re-calculate the rest of initial values
		if (!runtime::Eval(tr_->GetModelDatabase(), ct::Availability::kLiteral, 0,
						   tr_->layout(), tr_->GetTask()->reinit_bc.get(), &init))
			return false;
	}
	return job::Job(id_,
					tr_->dir(),
					dir_.get(),
					*tr_->GetTask(),
					nullptr,
					&init,
					isd_.get(), tr_->reader(), tr_->GetModelDatabase());
}

}
}
