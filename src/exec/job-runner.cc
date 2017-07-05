/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/job-runner.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>

#include "compiler.h"
#include "db/read-only-driver.h"
#include "exec/task-runner.h"
#include "job.h"
#include "runtime.h"
#include "task.h"

namespace flint {
namespace exec {

JobRunner::JobRunner(TaskRunner *tr, int id)
	: tr_(tr)
	, id_(id)
	, dir_(job::BuildPath(tr->dir(), id))
	, isd_(dir_ / "out.isd")
{
}

bool JobRunner::Run()
{
	std::vector<double> init(tr_->data()); // copy data
	{
		std::vector<double> generated_init;
		{
			compiler::Compiler c(tr_->GetDimensionAnalyzer());
			auto g = db::ReadOnlyDriver::Create(dir_ / "generated.db");
			std::unique_ptr<Bytecode> generated_bc(c.Compile(g->db(), "parameter_eqs", compiler::Method::kAssign));
			if (!generated_bc)
				return false;
			// TODO: give a proper seed if desired
			if (!runtime::Eval(tr_->GetDatabase(), ct::Availability::kNone, 0,
							   tr_->generated_layout(), generated_bc.get(),
							   nullptr, nullptr, &generated_init))
				return false;
		}
		if (!job::Store(tr_->GetDatabase(), tr_->generated_layout(), generated_init.data(), tr_->layout(), init.data()))
			return false;
	}
	if (tr_->GetTask()->reinit_bc) {
		// Re-calculate the rest of initial values
		if (!runtime::Eval(tr_->GetModelDatabase(), ct::Availability::kLiteral, 0,
						   tr_->layout(), tr_->GetTask()->reinit_bc.get(),
						   &tr_->GetTask()->inbound, &tr_->GetTask()->tv, &init))
			return false;
	}
	return job::Job(id_,
					tr_->dir(),
					dir_,
					*tr_->GetTask(),
					nullptr,
					&init,
					isd_);
}

}
}
