/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/ct.h"

#include <cassert>
#include <iostream>
#include <set>
#include <string>
#include <unordered_set>

#include "bc.pb.h"

#include "bc/locater.h"
#include "bc/mounter.h"
#include "lo/layout.h"
#include "runtime/calculation-dependency.h"
#include "runtime/flow.h"
#include "runtime/section-context.h"

namespace flint {
namespace ct {

DataFlowAnalyzer::DataFlowAnalyzer(const Layout *layout, int layer_size, Bytecode *bytecode)
	: layout_(layout)
	, layer_size_(layer_size)
	, bytecode_(bytecode)
	, euv_()
	, code_offset_()
{}

bool DataFlowAnalyzer::IsEmpty() const
{
	return bytecode_->cv->empty();
}

void DataFlowAnalyzer::CalculateCodeOffset()
{
	code_offset_.reset(new int[GetBhv().size()]);
	int bi = 0;
	int ci = 0;
	for (const auto &bh : GetBhv()) {
		code_offset_[bi++] = ci;
		ci += bh.noc();
	}
}

bool DataFlowAnalyzer::SolveLocation()
{
	auto sit = GetShv().begin();
	auto bit = GetBhv().begin();
	auto cit = GetCv().begin();
	while (sit != GetShv().end()) {
		int nob = sit->nob();
		const Locater *locater = layout_->GetLocater(sit->id());
		if (!locater) {
			// no such sector, so let's remove the section
			for (int i=0;i<nob;i++) {
				CVector::iterator cend = cit + bit->noc();
				cit = GetCv().erase(cit, cend);
				bit = GetBhv().erase(bit);
			}
			sit = GetShv().erase(sit);
			continue;
		}
		BhVector::iterator bend = bit + nob;
		while (bit != bend) {
			const bc::BlockHeader &bh = *bit++;
			CVector::iterator cend = cit + bh.noc();
			while (cit != cend) {
				bc::Code &code = *cit++;
				switch (code.type()) {
				case bc::Code::kLb:
					{
						bc::Lb *lb = code.mutable_lb();
						int so;
						if (!locater->Find(lb->v(), &so)) {
							runtime::ReportSectionContext(*sit);
							return false;
						}
						lb->set_so(so);
					}
					break;
				case bc::Code::kLoad:
					{
						bc::Load *load = code.mutable_load();
						int so, lo;
						if (!locater->Find(load->v(), &so, &lo)) {
							runtime::ReportSectionContext(*sit);
							return false;
						}
						load->set_so(so);
						load->set_lo(lo);
						//load->clear_v(); // keep the name for later call
					}
					break;
				case bc::Code::kStore:
					{
						bc::Store *store = code.mutable_store();
						int so, lo;
						if (!locater->Find(store->v(), &so, &lo)) {
							runtime::ReportSectionContext(*sit);
							return false;
						}
						store->set_so(so);
						store->set_lo(lo);
						//store->clear_v(); // keep the name for later call
					}
					break;
				case bc::Code::kRefer:
					{
						bc::Refer *refer = code.mutable_refer();
						int so, lo;
						if (!locater->Find(refer->v(), &so, &lo)) {
							runtime::ReportSectionContext(*sit);
							return false;
						}
						refer->set_so(so);
						refer->set_lo(lo);
						//refer->clear_v(); // keep the name for later call
					}
					break;
				case bc::Code::kSave:
					{
						bc::Save *save = code.mutable_save();
						int so, lo;
						if (!locater->Find(save->v(), &so, &lo)) {
							runtime::ReportSectionContext(*sit);
							return false;
						}
						save->set_so(so);
						save->set_lo(lo);
						//save->clear_v(); // keep the name for later call
					}
					break;
				default:
					// skip
					break;
				}
			}
		}
		sit++;
	}
	assert(bit == GetBhv().end());
	assert(cit == GetCv().end());
	return true;
}

int DataFlowAnalyzer::GetMaxNoir()
{
	int max_noir = 0;
	for (const auto &bh : GetBhv()) {
		max_noir = std::max(max_noir, bh.noir());
	}
	return max_noir;
}

int DataFlowAnalyzer::GetMaxNumberOfData()
{
	int max_nod = 0;
	for (const auto &bh : GetBhv()) {
		max_nod = std::max(max_nod, bh.nod());
	}
	return max_nod;
}

bool DataFlowAnalyzer::SolveDependencies(const FlowInboundMap *inbound,
										 Availability availability,
										 size_t *color)
{
	std::unique_ptr<CalculationDependencyVector> cdv(new CalculationDependencyVector);
	CollectCalculationDependencies(cdv.get());

	std::unique_ptr<ReductionUnitVector> ruv(new ReductionUnitVector);
	if (inbound)
		CollectReductionUnits(inbound, ruv.get());

	std::unique_ptr<int[]> levels(new int[GetNol() * layer_size_]());
	switch (availability) {
	case Availability::kNone:
		// nothing to do
		break;
	case Availability::kLiteral:
		layout_->MarkLiteral(GetNol(), layer_size_, levels.get(), color);
		break;
	case Availability::kConstant:
		layout_->MarkConstant(GetNol(), layer_size_, levels.get());
		break;
	}
	int m = 0;
	while (!cdv->empty() || !ruv->empty()) {
		size_t n = 0;
		++m;

		// Level 2M
		CalculationDependencyVector::iterator cit = cdv->begin();
		while (cit != cdv->end()) {
			const std::unordered_set<int> &la = (*cit)->load_addrs();
			if (std::all_of(la.begin(), la.end(),
							[&levels, m](int i){return 1 <= levels[i] && levels[i] < 2*m;})) {
				euv_.push_back((*cit)->cu());
				levels[(*cit)->store_addr()] = 2*m;
				n++;
				cit = cdv->erase(cit);
			} else {
				++cit;
			}
		}

		// Level 2M+1
		ReductionUnitVector::iterator rit = ruv->begin();
		while (rit != ruv->end()) {
			const std::unordered_set<int> &sa = (*rit)->source_addrs();
			if (std::all_of(sa.begin(), sa.end(),
							[&levels, m](int i){return 1 <= levels[i] && levels[i] <= 2*m;})) {
				euv_.push_back(**rit);
				levels[(*rit)->target_addr()] = 2*m+1;
				n++;
				rit = ruv->erase(rit);
			} else {
				++rit;
			}
		}

		if (n == 0) {
			std::cerr << "failed to solve the rest of dependencies: "
					  << static_cast<int>(availability)
					  << ", "
					  << cdv->size()
					  << "/"
					  << ruv->size()
					  << std::endl;
			for (const auto &cdp : *cdv) {
				const std::unordered_set<int> &la = cdp->load_addrs();
				for (auto lait=la.cbegin();lait!=la.cend();++lait) {
					if (lait == la.cbegin()) {
						std::cerr << *lait;
					} else {
						std::cerr << "," << *lait;
					}
					// append a question mark if the address is unprepared
					if (levels[*lait] == 0) {
						std::cerr << "?";
					}
				}
				std::cerr << " -> " << cdp->store_addr() << std::endl;
			}
			for (const auto &rup : *ruv) {
				std::cerr << "target_addr: " << rup->target_addr()
						  << " <- ";
				for (auto sit=rup->source_addrs().cbegin();sit!=rup->source_addrs().cend();++sit) {
					if (sit == rup->source_addrs().cbegin()) {
						std::cerr << *sit;
					} else {
						std::cerr << ", " << *sit;
					}
				}
				std::cerr << std::endl;
			}
			return false;
		}
	}
	return true;
}

void DataFlowAnalyzer::ScheduleEvents(const FlowInboundMap &inbound)
{
	// Collect target addresses in which events update values
	std::unordered_set<int> addrs;

	int si = 0; // section index
	int bi = 0; // block index
	int ci = 0; // code index

	int nos = static_cast<int>(GetShv().size());
	for (;si < nos; si++) {
		const bc::SectionHeader &sh(GetShv().at(si));
		int bib = bi;
		int bie = bi + sh.nob();
		const std::string &id(sh.id());
		const Mounter &mounter(layout_->GetMounter(id));
		int nos = mounter.size();
		for (int k=0;k<nos;k++) { // for each sector
			bi = bib; // reset block index
			int offset = mounter.GetOffset(k);
			for (;bi < bie; bi++) {
				const bc::BlockHeader &bh(GetBhv().at(bi));
				ci = code_offset_[bi];
				int cib = ci;
				int cie = cib + bh.noc();
				std::unique_ptr<CalculationUnit> cu(new CalculationUnit(si, bi, offset, cib, cie));
				for (;ci < cie; ci++) {
					const bc::Code &code(GetCv().at(ci));
					switch (code.type()) {
					case bc::Code::kStore:
						{
							const bc::Store &store = code.store();
							addrs.insert(offset + store.so());
						}
						break;
					case bc::Code::kSave:
						{
							const bc::Save &save = code.save();
							addrs.insert(offset + save.so()); // TODO: fix misnomer
						}
						break;
					default:
						break;
					}
				}
				euv_.push_back(*cu);
			}
		}
	}

	// Collect reductions which sources include any of addrs
	for (auto it=inbound.cbegin();it!=inbound.cend();++it) {
		std::unique_ptr<ReductionUnit> ru(new ReductionUnit(it->second.reduction,
															it->first,
															it->second.size));
		bool found = false;
		for (int src : it->second.sources) {
			ru->AddSourceAddr(src);
			if (!found && addrs.count(src))
				found = true;
		}
		if (found)
			euv_.push_back(*ru);
	}
}

void DataFlowAnalyzer::CollectCalculationDependencies(CalculationDependencyVector *cdv)
{
	assert(cdv);

	int si = 0; // section index
	int bi = 0; // block index
	int ci = 0; // code index

	int nos = static_cast<int>(GetShv().size());
	for (;si < nos; si++) {
		const bc::SectionHeader &sh(GetShv().at(si));
		int bib = bi;
		int bie = bi + sh.nob();
		const std::string &id(sh.id());
		const Mounter &mounter(layout_->GetMounter(id));
		int nos = mounter.size();
		for (int k=0;k<nos;k++) { // for each sector
			bi = bib; // reset block index
			int offset = mounter.GetOffset(k);
			for (;bi < bie; bi++) {
				const bc::BlockHeader &bh(GetBhv().at(bi));
				ci = code_offset_[bi];
				int cib = ci;
				int cie = cib + bh.noc();
				std::unique_ptr<CalculationDependency> cd(new CalculationDependency(si, bi, offset, cib, cie));
				for (;ci < cie; ci++) {
					const bc::Code &code(GetCv().at(ci));
					switch (code.type()) {
					case bc::Code::kLoad:
						{
							const bc::Load &load = code.load();
							switch (load.lo()) {
							case -1:
								// nothing to do
								break;
							case -2:
								// nothing to do
								break;
							default:
								{
									int addr = offset + load.so() + (layer_size_ * load.lo());
									cd->AddLoadAddress(addr);
								}
								break;
							}
						}
						break;
					case bc::Code::kStore:
						{
							const bc::Store &store = code.store();
							int addr = offset + store.so() + (layer_size_ * store.lo());
							cd->SetStoreAddr(addr);
						}
						break;
					case bc::Code::kRefer:
						{
							const bc::Refer &refer = code.refer();
							switch (refer.lo()) {
							case -1:
							case -2:
								// nothing to do
								break;
							default:
								{
									int addr = offset + refer.so() + (layer_size_ * refer.lo());
									cd->AddLoadAddress(addr); // TODO: fix misnomer
								}
								break;
							}
						}
						break;
					case bc::Code::kSave:
						{
							const bc::Save &save = code.save();
							int addr = offset + save.so() + (layer_size_ * save.lo());
							cd->SetStoreAddr(addr); // TODO: fix misnomer
						}
						break;
					default:
						break;
					}
				}

				cdv->push_back(std::move(cd));
			}
		}
	}
}

void DataFlowAnalyzer::CollectReductionUnits(const FlowInboundMap *inbound,
											 ReductionUnitVector *ruv)
{
	assert(inbound);
	assert(ruv);

	for (auto it=inbound->cbegin();it!=inbound->cend();++it) {
		for (int i=0;i<GetNol();i++) {
			if (i%2 == 0) { // only on an even layer
				std::unique_ptr<ReductionUnit> rd(new ReductionUnit(it->second.reduction,
																	it->first + (i * layer_size_),
																	it->second.size));
				for (int src : it->second.sources) {
					rd->AddSourceAddr(src + (i * layer_size_));
				}
				ruv->push_back(std::move(rd));
			}
		}
	}
}

int DataFlowAnalyzer::GetNol() const
{
	return bytecode_->nol;
}

ShVector &DataFlowAnalyzer::GetShv()
{
	return *bytecode_->shv;
}

BhVector &DataFlowAnalyzer::GetBhv()
{
	return *bytecode_->bhv;
}

CVector &DataFlowAnalyzer::GetCv()
{
	return *bytecode_->cv;
}

}
}
