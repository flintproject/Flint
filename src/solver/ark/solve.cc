/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "solver/ark.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>

#include "cas.h"
#include "flint/bc.h"
#include "job.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "runtime/flow.h"
#include "runtime/processor.h"
#include "solver.h"
#include "solver/ark/ark.h"
#include "solver/ark/auxv.h"
#include "solver/ark/mass.h"
#include "solver/ark/mmdm.h"
#include "solver/ark/rhs.h"

using std::cerr;
using std::endl;

namespace flint {
namespace solver {
namespace ark {

namespace {

Processor *CreateProcessor(const Layout *layout, size_t layer_size,
						   FlowInboundMap *inbound, const char *file)
{
	std::unique_ptr<Processor> processor(new Processor(layout, layer_size));
	int nol = 0;
	if (!LoadBytecode(file, &nol, processor.get()))
		return nullptr;
	assert(nol <= 2);
	if (!processor->SolveLocation())
		return nullptr;
	processor->CalculateCodeOffset();
	if (!processor->SolveDependencies(nol, inbound))
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

	std::unique_ptr<Layout> layout(new Layout);
	{
		LayoutLoader loader(option.layout_file);
		if (!loader.Load(layout.get()))
			return false;
	}
	size_t layer_size = layout->Calculate();

	size_t dirname_size = std::strlen(option.task_dir);
	assert(dirname_size > 0);
	std::unique_ptr<char[]> auxv_bc_file(new char[dirname_size+32]);
	std::sprintf(auxv_bc_file.get(), "%s/auxv-bc", option.task_dir);
	if (!system->SaveAuxVarBc(auxv_bc_file.get()))
		return false;
	std::unique_ptr<char[]> mass_bc_file(new char[dirname_size+32]);
	std::sprintf(mass_bc_file.get(), "%s/mass-bc", option.task_dir);
	if (!system->SaveOdeMassBc(mass_bc_file.get()))
		return false;
	std::unique_ptr<char[]> rhs_bc_file(new char[dirname_size+32]);
	std::sprintf(rhs_bc_file.get(), "%s/rhs-bc", option.task_dir);
	if (!system->SaveOdeRhsBc(rhs_bc_file.get()))
		return false;

	std::unique_ptr<Processor> auxv_proc(CreateProcessor(layout.get(), layer_size,
														 inbound.get(), auxv_bc_file.get()));
	if (!auxv_proc)
		return false;
	std::unique_ptr<Auxv> auxv(new Auxv(auxv_proc.get()));

	std::unique_ptr<Processor> mass_proc(CreateProcessor(layout.get(), layer_size,
														 inbound.get(), mass_bc_file.get()));
	if (!mass_proc)
		return false;
	std::unique_ptr<Mmdm> mmdm(new Mmdm(layout->SelectStates()));
	if (!layout->GenerateMmdm(*system, mmdm.get()))
		return false;
	std::unique_ptr<Mass> mass(new Mass(mass_proc.get(), mmdm.get()));

	std::unique_ptr<Processor> rhs_proc(CreateProcessor(layout.get(), layer_size,
														inbound.get(), rhs_bc_file.get()));
	if (!rhs_proc)
		return false;
	std::unique_ptr<Rhs> rhs(new Rhs(layer_size, rhs_proc.get()));

	std::unique_ptr<Ark> ark(new Ark(layout.get(), layer_size,
									 auxv.get(), mass.get(), rhs.get()));
	return ark->Solve(option);
}

}
}
}
