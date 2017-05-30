/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "solver/ark.h"

#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>

#include "cas.h"
#include "flint/bc.h"
#include "job.h"
#include "lo/layout.h"
#include "runtime/flow.h"
#include "runtime/processor.h"
#include "solver.h"
#include "solver/ark/ark.h"
#include "solver/ark/auxv.h"
#include "solver/ark/mass.h"
#include "solver/ark/mmdm.h"
#include "solver/ark/rhs.h"
#include "task.h"

namespace flint {
namespace solver {
namespace ark {

namespace {

Processor *CreateProcessor(const task::Task &task, Bytecode *bytecode)
{
	std::unique_ptr<Processor> processor(new Processor(task.layout.get(), task.layer_size, bytecode, &task.tv));
	assert(bytecode->nol <= 2);
	if (!processor->SolveLocation())
		return nullptr;
	processor->CalculateCodeOffset();
	if (!processor->SolveDependencies(&task.inbound))
		return nullptr;
	return processor.release();
}

}

bool Solve(sqlite3 *db, task::Task &task, const job::Option &option)
{
	std::unique_ptr<cas::System> system(new cas::System);
	if (!system->Load(db))
		return false;
	if (!cas::AnnotateEquations(db, "input_eqs", system.get()))
		return false;

	std::unique_ptr<Bytecode> auxv_bc(system->GenerateAuxVarBc());
	if (!auxv_bc)
		return false;
	std::unique_ptr<Bytecode> mass_bc(system->GenerateOdeMassBc());
	if (!mass_bc)
		return false;
	std::unique_ptr<Bytecode> rhs_bc(system->GenerateOdeRhsBc());
	if (!rhs_bc)
		return false;

	std::unique_ptr<Processor> auxv_proc(CreateProcessor(task, auxv_bc.get()));
	if (!auxv_proc)
		return false;
	std::unique_ptr<Auxv> auxv(new Auxv(auxv_proc.get()));

	std::unique_ptr<Processor> mass_proc(CreateProcessor(task, mass_bc.get()));
	if (!mass_proc)
		return false;
	std::unique_ptr<Mmdm> mmdm(new Mmdm(task.layout->SelectStates()));
	if (!task.layout->GenerateMmdm(*system, mmdm.get()))
		return false;
	std::unique_ptr<Mass> mass(new Mass(mass_proc.get(), mmdm.get()));

	std::unique_ptr<Processor> rhs_proc(CreateProcessor(task, rhs_bc.get()));
	if (!rhs_proc)
		return false;
	std::unique_ptr<Rhs> rhs(new Rhs(task.layer_size, rhs_proc.get()));

	std::unique_ptr<Ark> ark(new Ark(task.layout.get(), task.layer_size,
									 auxv.get(), mass.get(), rhs.get()));
	return ark->Solve(task, option);
}

}
}
}
