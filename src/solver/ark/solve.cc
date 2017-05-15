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

namespace flint {
namespace solver {
namespace ark {

namespace {

Processor *CreateProcessor(const Layout *layout, size_t layer_size,
						   FlowInboundMap *inbound, Bytecode *bytecode)
{
	std::unique_ptr<Processor> processor(new Processor(layout, layer_size, bytecode));
	assert(bytecode->nol <= 2);
	if (!processor->SolveLocation())
		return nullptr;
	processor->CalculateCodeOffset();
	if (!processor->SolveDependencies(inbound))
		return nullptr;
	return processor.release();
}

}

bool Solve(sqlite3 *db, const job::Option &option)
{
	std::unique_ptr<cas::System> system(new cas::System);
	if (!system->Load(db))
		return false;
	if (!cas::AnnotateEquations(db, "input_eqs", system.get()))
		return false;
	std::unique_ptr<FlowInboundMap> inbound(new FlowInboundMap);
	if (!LoadFlows(db, inbound.get()))
		return false;

	size_t dirname_size = std::strlen(option.task_dir);
	assert(dirname_size > 0);
	std::unique_ptr<Bytecode> auxv_bc(system->GenerateAuxVarBc());
	if (!auxv_bc)
		return false;
	std::unique_ptr<Bytecode> mass_bc(system->GenerateOdeMassBc());
	if (!mass_bc)
		return false;
	std::unique_ptr<Bytecode> rhs_bc(system->GenerateOdeRhsBc());
	if (!rhs_bc)
		return false;

	std::unique_ptr<Processor> auxv_proc(CreateProcessor(option.layout.get(), option.layer_size,
														 inbound.get(), auxv_bc.get()));
	if (!auxv_proc)
		return false;
	std::unique_ptr<Auxv> auxv(new Auxv(auxv_proc.get()));

	std::unique_ptr<Processor> mass_proc(CreateProcessor(option.layout.get(), option.layer_size,
														 inbound.get(), mass_bc.get()));
	if (!mass_proc)
		return false;
	std::unique_ptr<Mmdm> mmdm(new Mmdm(option.layout->SelectStates()));
	if (!option.layout->GenerateMmdm(*system, mmdm.get()))
		return false;
	std::unique_ptr<Mass> mass(new Mass(mass_proc.get(), mmdm.get()));

	std::unique_ptr<Processor> rhs_proc(CreateProcessor(option.layout.get(), option.layer_size,
														inbound.get(), rhs_bc.get()));
	if (!rhs_proc)
		return false;
	std::unique_ptr<Rhs> rhs(new Rhs(option.layer_size, rhs_proc.get()));

	std::unique_ptr<Ark> ark(new Ark(option.layout.get(), option.layer_size,
									 auxv.get(), mass.get(), rhs.get()));
	return ark->Solve(option);
}

}
}
}
