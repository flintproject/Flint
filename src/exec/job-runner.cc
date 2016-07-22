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

using std::cerr;
using std::endl;
using std::sprintf;

namespace flint {
namespace exec {

namespace {
const int kFilenameLength = 96;
}

JobRunner::JobRunner(TaskRunner *tr, int id)
	: tr_(tr)
	, progress_address_(tr->GetProgressAddress(id))
	, dir_(job::BuildPath(tr->dir(), id))
	, generated_bc_(new char[kFilenameLength])
	, generated_db_(new char[kFilenameLength])
	, generated_init_(new char[kFilenameLength])
	, stored_(new char[kFilenameLength])
	, reinit_(new char[kFilenameLength])
	, isd_(new char[kFilenameLength])
{
	sprintf(generated_bc_.get(), "%s/generated-bc", dir_.get());
	sprintf(generated_db_.get(), "%s/generated.db", dir_.get());
	sprintf(generated_init_.get(), "%s/generated-init", dir_.get());
	sprintf(stored_.get(), "%s/stored", dir_.get());
	sprintf(reinit_.get(), "%s/reinit", dir_.get());
	sprintf(isd_.get(), "%s/out.isd", dir_.get());
}

bool JobRunner::Run()
{
	{
		compiler::Compiler c(tr_->GetDimensionAnalyzer());
		db::ReadOnlyDriver g(generated_db_.get());
		if (!c.Compile(g.db(), "parameter_eqs", compiler::Method::kAssign, generated_bc_.get()))
			return false;
	}
	// TODO: give a proper seed if desired
	if (!runtime::Eval(tr_->GetDatabase(), ct::Availability::kNone, 0,
					   tr_->generated_layout(), generated_bc_.get(), generated_init_.get()))
		return false;
	{
		boost::system::error_code ec;
		boost::filesystem::copy_file(tr_->init(), stored_.get(), ec);
		if (ec) {
			cerr << "failed to copy " << tr_->init()
				 << " to " << stored_.get()
				 << ": " << ec << endl;
			return false;
		}
	}
	if (!job::Store(tr_->GetDatabase(), tr_->generated_layout(), generated_init_.get(), tr_->layout(), stored_.get()))
		return false;
	if (tr_->reinit_bc()) {
		// Re-calculate the rest of initial values
		if (!runtime::Eval(tr_->GetModelDatabase(), ct::Availability::kLiteral, 0,
						   tr_->layout(), tr_->reinit_bc(), reinit_.get(), stored_.get()))
		return false;
	}
	return job::Job(tr_->dir(), dir_.get(), progress_address_,
					tr_->reinit_bc() ? reinit_.get() : stored_.get(),
					isd_.get(), tr_->reader(), tr_->GetModelDatabase());
}

}
}
