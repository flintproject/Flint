/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "solver/ark.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <iostream>
#include <memory>

#include "cas.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "runtime/flow.hh"
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
	{
		std::unique_ptr<BcLoader> loader(new BcLoader(file));
		if (!loader->Load(&nol, processor.get()))
			return nullptr;
	}
	assert(nol <= 2);
	if (!processor->SolveLocation())
		return nullptr;
	processor->CalculateCodeOffset();
	if (!processor->SolveDependencies(nol, inbound, true))
		return nullptr;
	return processor.release();
}

}

bool Solve(sqlite3 *db, const Option &option)
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

	const char *auxv_bc_file = "auxv-bc";
	if (!system->SaveAuxVarBc(auxv_bc_file))
		return false;
	const char *mass_bc_file = "mass-bc";
	if (!system->SaveOdeMassBc(mass_bc_file))
		return false;
	const char *rhs_bc_file = "rhs-bc";
	if (!system->SaveOdeRhsBc(rhs_bc_file))
		return false;

	std::unique_ptr<Processor> auxv_proc(CreateProcessor(layout.get(), layer_size,
														 inbound.get(), auxv_bc_file));
	if (!auxv_proc)
		return false;
	std::unique_ptr<Auxv> auxv(new Auxv(auxv_proc.get()));

	std::unique_ptr<Processor> mass_proc(CreateProcessor(layout.get(), layer_size,
														 inbound.get(), mass_bc_file));
	if (!mass_proc)
		return false;
	std::unique_ptr<Mmdm> mmdm(new Mmdm(layout->SelectStates()));
	if (!layout->GenerateMmdm(*system, mmdm.get()))
		return false;
	std::unique_ptr<Mass> mass(new Mass(mass_proc.get(), mmdm.get()));

	std::unique_ptr<Processor> rhs_proc(CreateProcessor(layout.get(), layer_size,
														inbound.get(), rhs_bc_file));
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
