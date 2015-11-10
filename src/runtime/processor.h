/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_PROCESSOR_H_
#define FLINT_RUNTIME_PROCESSOR_H_

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <iterator>
#include <memory>
#include <random>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "bc.pb.h"

#include "bc/bc_loader.h"
#include "lo/layout.h"
#include "numeric/prng.h"
#include "runtime/flow.hh"
#include "runtime/timeseries.h"

#include "calculation-dependency.hh"
#include "execution-unit.hh"

namespace flint {
namespace {

bool IsFinite(double v, int offset)
{
	switch (std::fpclassify(v)) {
	case FP_NAN:
		std::cerr << "NaN happened at " << offset << ", stopped" << std::endl;
		return false;
	case FP_INFINITE:
		if (v > 0) {
			std::cerr << "+Inf happened at " << offset << ", stopped" << std::endl;
		} else {
			std::cerr << "-Inf happened at " << offset << ", stopped" << std::endl;
		}
		return false;
	default: // FP_ZERO/FP_NORMAL/FP_SUBNORMAL
		return true;
	}
}

void DoCall1(const bc::Call1 &c1, double *tmp)
{
	int a = c1.a();
	int a1 = c1.a1();
	switch (c1.op()) {
	case bc::Call1::kAbs:
		tmp[a] = std::fabs(tmp[a1]);
		break;
	case bc::Call1::kArccos:
		tmp[a] = std::acos(tmp[a1]);
		break;
	case bc::Call1::kArccosh:
		tmp[a] = std::acosh(tmp[a1]);
		break;
	case bc::Call1::kArccot:
		tmp[a] = std::atan(1/tmp[a1]);
		break;
	case bc::Call1::kArccoth:
		tmp[a] = std::atanh(1/tmp[a1]);
		break;
	case bc::Call1::kArccsc:
		tmp[a] = std::asin(1/tmp[a1]);
		break;
	case bc::Call1::kArccsch:
		tmp[a] = std::asinh(1/tmp[a1]);
		break;
	case bc::Call1::kArcsec:
		tmp[a] = std::acos(1/tmp[a1]);
		break;
	case bc::Call1::kArcsech:
		tmp[a] = std::acosh(1/tmp[a1]);
		break;
	case bc::Call1::kArcsin:
		tmp[a] = std::asin(tmp[a1]);
		break;
	case bc::Call1::kArcsinh:
		tmp[a] = std::asinh(tmp[a1]);
		break;
	case bc::Call1::kArctan:
		tmp[a] = std::atan(tmp[a1]);
		break;
	case bc::Call1::kArctanh:
		tmp[a] = std::atanh(tmp[a1]);
		break;
	case bc::Call1::kCeiling:
		tmp[a] = std::ceil(tmp[a1]);
		break;
	case bc::Call1::kCos:
		tmp[a] = std::cos(tmp[a1]);
		break;
	case bc::Call1::kCosh:
		tmp[a] = std::cosh(tmp[a1]);
		break;
	case bc::Call1::kCot:
		tmp[a] = 1/std::tan(tmp[a1]);
		break;
	case bc::Call1::kCoth:
		tmp[a] = 1/std::tanh(tmp[a1]);
		break;
	case bc::Call1::kCsc:
		tmp[a] = 1/std::sin(tmp[a1]);
		break;
	case bc::Call1::kCsch:
		tmp[a] = 1/std::sinh(tmp[a1]);
		break;
	case bc::Call1::kExp:
		tmp[a] = std::exp(tmp[a1]);
		break;
	case bc::Call1::kFloor:
		tmp[a] = std::floor(tmp[a1]);
		break;
	case bc::Call1::kLn:
		tmp[a] = std::log(tmp[a1]);
		break;
	case bc::Call1::kLog10:
		tmp[a] = std::log10(tmp[a1]);
		break;
	case bc::Call1::kMinus1:
		tmp[a] = -tmp[a1];
		break;
	case bc::Call1::kRoot1:
		tmp[a] = std::sqrt(tmp[a1]);
		break;
	case bc::Call1::kSec:
		tmp[a] = 1/std::cos(tmp[a1]);
		break;
	case bc::Call1::kSech:
		tmp[a] = 1/std::cosh(tmp[a1]);
		break;
	case bc::Call1::kSin:
		tmp[a] = std::sin(tmp[a1]);
		break;
	case bc::Call1::kSinh:
		tmp[a] = std::sinh(tmp[a1]);
		break;
	case bc::Call1::kTan:
		tmp[a] = std::tan(tmp[a1]);
		break;
	case bc::Call1::kTanh:
		tmp[a] = std::tanh(tmp[a1]);
		break;
	}
}

void DoCall2(const bc::Call2 &c2, double *tmp)
{
	int a = c2.a();
	int a1 = c2.a1();
	int a2 = c2.a2();
	switch (c2.op()) {
	case bc::Call2::kDivide:
		tmp[a] = tmp[a1] / tmp[a2];
		break;
	case bc::Call2::kEq:
		tmp[a] = tmp[a1] == tmp[a2];
		break;
	case bc::Call2::kGeq:
		tmp[a] = tmp[a1] >= tmp[a2];
		break;
	case bc::Call2::kGt:
		tmp[a] = tmp[a1] > tmp[a2];
		break;
	case bc::Call2::kLeq:
		tmp[a] = tmp[a1] <= tmp[a2];
		break;
	case bc::Call2::kLog:
		tmp[a] = std::log(tmp[a2])/std::log(tmp[a1]);
		break;
	case bc::Call2::kLt:
		tmp[a] = tmp[a1] < tmp[a2];
		break;
	case bc::Call2::kMax:
		tmp[a] = std::max(tmp[a1], tmp[a2]);
		break;
	case bc::Call2::kMin:
		tmp[a] = std::min(tmp[a1], tmp[a2]);
		break;
	case bc::Call2::kMinus2:
		tmp[a] = tmp[a1] - tmp[a2];
		break;
	case bc::Call2::kNeq:
		tmp[a] = tmp[a1] != tmp[a2];
		break;
	case bc::Call2::kPlus:
		tmp[a] = tmp[a1] + tmp[a2];
		break;
	case bc::Call2::kPower:
		tmp[a] = std::pow(tmp[a1], tmp[a2]);
		break;
	case bc::Call2::kRemainder:
		tmp[a] = std::fmod(tmp[a1], tmp[a2]);
		break;
	case bc::Call2::kRoot2:
		// TODO
		std::cerr << "root with degree is not yet supported" << std::endl;
		break;
	case bc::Call2::kTimes:
		tmp[a] = tmp[a1] * tmp[a2];
		break;
	}
}

template<typename TRng>
void DoGen1(const bc::Gen1 &c1, double *tmp, TRng *rng)
{
	int a = c1.a();
	int a1 = c1.a1();
	switch (c1.type()) {
	case bc::Gen1::kExponentialVariate:
		tmp[a] = GetExponentialVariate(tmp[a1], rng);
		break;
	case bc::Gen1::kPoissonVariate:
		tmp[a] = GetPoissonVariate(tmp[a1], rng);
		break;
	}
}

template<typename TRng>
void DoGen2(const bc::Gen2 &c2, double *tmp, TRng *rng)
{
	int a = c2.a();
	int a1 = c2.a1();
	int a2 = c2.a2();
	switch (c2.type()) {
	case bc::Gen2::kGammaVariate:
		tmp[a] = GetGammaVariate(tmp[a1], tmp[a2], rng);
		break;
	case bc::Gen2::kGaussVariate:
		tmp[a] = GetGaussVariate(tmp[a1], tmp[a2], rng);
		break;
	case bc::Gen2::kLognormalVariate:
		tmp[a] = GetLognormalVariate(tmp[a1], tmp[a2], rng);
		break;
	case bc::Gen2::kUniformVariate:
		tmp[a] = GetUniformVariate(tmp[a1], tmp[a2], rng);
		break;
	case bc::Gen2::kWeibullVariate:
		tmp[a] = GetWeibullVariate(tmp[a1], tmp[a2], rng);
		break;
	}
}

} // namespace

typedef std::vector<std::unique_ptr<ReductionUnit> > ReductionUnitVector;

typedef boost::ptr_vector<CalculationDependency> CalculationDependencyVector;

class Processor {
public:
	Processor(const Processor &) = delete;
	Processor &operator=(const Processor &) = delete;

	Processor(const Layout *layout, int layer_size)
		: layout_(layout), layer_size_(layer_size),
		  shv_(new ShVector), bhv_(new BhVector), cv_(new CVector),
		  euv_(),
		  code_offset_(), tmp_(NULL), tv_(NULL), rng_(NULL) {}

	ShVector *GetShv() const {return shv_.get();}
	BhVector *GetBhv() const {return bhv_.get();}
	CVector *GetCv() const {return cv_.get();}

	void set_tmp(double *tmp) {tmp_ = tmp;}
	void set_tv(TimeseriesVector *tv) {tv_ = tv;}
	void set_rng(std::mt19937 *rng) {rng_ = rng;}

	void CalculateCodeOffset() {
		code_offset_.reset(new int[bhv_->size()]);
		int bi = 0;
		int ci = 0;
		for (BhVector::const_iterator it=bhv_->begin();it!=bhv_->end();++it) {
			code_offset_[bi++] = ci;
			ci += it->noc();
		}
	}

	/*
	 * This function drops some elements of shv_, bhv_, and cv_ not associated
	 * with any existing sector, thus should be called first.
	 */
	bool SolveLocation() {
		ShVector::iterator sit = shv_->begin();
		BhVector::iterator bit = bhv_->begin();
		CVector::iterator cit = cv_->begin();
		while (sit != shv_->end()) {
			int nob = sit->nob();
			const Locater *locater = layout_->GetLocater(sit->id());
			if (!locater) {
				// no such sector, so let's remove the section
				for (int i=0;i<nob;i++) {
					CVector::iterator cend = cit + bit->noc();
					cit = cv_->erase(cit, cend);
					bit = bhv_->erase(bit);
				}
				sit = shv_->erase(sit);
				continue;
			}
			sit++;
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
								return false;
							}
							lb->set_so(so);
							lb->clear_v();
						}
						break;
					case bc::Code::kLoad:
						{
							bc::Load *load = code.mutable_load();
							int so, lo;
							if (!locater->Find(load->v(), &so, &lo)) {
								return false;
							}
							load->set_so(so);
							load->set_lo(lo);
							load->clear_v();
						}
						break;
					case bc::Code::kStore:
						{
							bc::Store *store = code.mutable_store();
							int so, lo;
							if (!locater->Find(store->v(), &so, &lo)) {
								return false;
							}
							store->set_so(so);
							store->set_lo(lo);
							store->clear_v();
						}
						break;
					default:
						// skip
						break;
					}
				}
			}
		}
		assert(bit == bhv_->end());
		assert(cit == cv_->end());
		return true;
	}

	int GetMaxNumberOfData() {
		int max_nod = 0;
		for (BhVector::const_iterator it=bhv_->begin();it!=bhv_->end();++it) {
			max_nod = std::max(max_nod, it->nod());
		}
		return max_nod;
	}

	bool SolveDependencies(int nol,
						   const FlowInboundMap *inbound,
						   bool constantAvailable) {
		std::unique_ptr<CalculationDependencyVector> cdv(new CalculationDependencyVector);
		CollectCalculationDependencies(cdv.get());

		std::unique_ptr<ReductionUnitVector> ruv(new ReductionUnitVector);
		CollectReductionUnits(nol, inbound, ruv.get());

		std::unique_ptr<char[]> ready_addresses(new char[nol * layer_size_]());
		if (constantAvailable)
			layout_->MarkConstant(nol, layer_size_, ready_addresses.get());
		while (!cdv->empty() || !ruv->empty()) {
			size_t n = 0;

			// Level 2N
			CalculationDependencyVector::iterator cit = cdv->begin();
			while (cit != cdv->end()) {
				const std::unordered_set<int> &la = cit->load_addrs();
				if (std::all_of(la.begin(), la.end(),
								[&ready_addresses](int i){return ready_addresses[i] == 1;})) {
					euv_.push_back(cit->cu());
					ready_addresses[cit->store_addr()] = 1;
					n++;
					cit = cdv->erase(cit);
				} else {
					++cit;
				}
			}

			// Level 2N+1
			ReductionUnitVector::iterator rit = ruv->begin();
			while (rit != ruv->end()) {
				const std::unordered_set<int> &sa = (*rit)->source_addrs();
				if (std::all_of(sa.begin(), sa.end(),
								[&ready_addresses](int i){return ready_addresses[i] == 1;})) {
					euv_.push_back(**rit);
					ready_addresses[(*rit)->target_addr()] = 1;
					n++;
					rit = ruv->erase(rit);
				} else {
					++rit;
				}
			}

			if (n == 0) {
				std::cerr << "failed to solve the rest of dependencies: "
					 << cdv->size()
					 << "/"
					 << ruv->size()
					 << std::endl;
				for (auto cit=cdv->cbegin();cit!=cdv->cend();++cit) {
					const std::unordered_set<int> &la = cit->load_addrs();
					for (auto lait=la.cbegin();lait!=la.cend();++lait) {
						if (lait == la.cbegin()) {
							std::cerr << *lait;
						} else {
							std::cerr << "," << *lait;
						}
						// append a question mark if the address is unprepared
						if (ready_addresses[*lait] == 0) {
							std::cerr << "?";
						}
					}
					std::cerr << " -> " << cit->store_addr() << std::endl;
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

	template<typename TExecutor>
	bool Process(TExecutor *executor) {
		for (const ExecutionUnit &eu : euv_) {
			if (eu.which() == 0) {
				const CalculationUnit &cu = boost::get<CalculationUnit>(eu);
				int si = cu.section_index();
				int k = cu.sector_index();
				int bi = cu.block_index();

			const bc::SectionHeader &sh(shv_->at(si));
			const std::string &id(sh.id());
			const Mounter &mounter(layout_->GetMounter(id));
			int nos = mounter.size();
			assert(k < nos);
			int offset = mounter.GetOffset(k);
			const bc::BlockHeader &bh(bhv_->at(bi));
			int ci = code_offset_[bi];
			int cib = ci;
			int cie = cib + bh.noc();
			while (ci < cie) {
				const bc::Code &code(cv_->at(ci));
				switch (code.type()) {
				case bc::Code::kBr:
					{
						const bc::Br &br(code.br());
						if (tmp_[br.a()]) {
							ci = cib + br.p();
						} else {
							ci++;
						}
					}
					break;
				case bc::Code::kJmp:
					ci = cib + code.jmp().p();
					break;
				case bc::Code::kCall1:
					DoCall1(code.call1(), tmp_);
					ci++;
					break;
				case bc::Code::kCall2:
					DoCall2(code.call2(), tmp_);
					ci++;
					break;
				case bc::Code::kLb:
					if (executor->Lb(code.lb(), offset)) {
						ci++;
					} else {
						std::cerr << "failed to look back: " << ci << std::endl;
						return false;
					}
					break;
				case bc::Code::kLd:
					if (DoLd(code.ld())) {
						ci++;
					} else {
						std::cerr << "failed to load data: " << ci << std::endl;
						return false;
					}
					break;
				case bc::Code::kGen1:
					DoGen1(code.gen1(), tmp_, rng_);
					ci++;
					break;
				case bc::Code::kGen2:
					DoGen2(code.gen2(), tmp_, rng_);
					ci++;
					break;
				case bc::Code::kLoad:
					if (executor->Load(code.load(), offset)) {
						ci++;
					} else {
						std::cerr << "failed to load: " << ci << std::endl;
						return false;
					}
					break;
				case bc::Code::kLoadi:
					{
						const bc::Loadi &loadi(code.loadi());
						tmp_[loadi.a()] = loadi.v();
						ci++;
					}
					break;
				case bc::Code::kRet:
					ci = cie;
					break;
				case bc::Code::kStore:
					double v = executor->Store(code.store(), offset);
					if (!IsFinite(v, offset)) {
						boost::uuids::uuid u;
						std::memcpy(&u, sh.id().data(), u.size());
						std::cerr << "section: " << u << std::endl;
						std::cerr << "block:   " << bh.name() << std::endl;
						std::cerr << "index:   " << (ci - cib) << std::endl;
						std::cerr << "registers:" << std::endl;
						for (int i=0;i<bh.nod();i++) {
							std::cerr << "$" << i << " = " << tmp_[i] << std::endl;
						}
						return false;
					}
					ci++;
					break;
				}
			}
			} else {
				const ReductionUnit &ru = boost::get<ReductionUnit>(eu);
				if (!executor->Reduce(ru))
					return false;
			}
		}
		return true;
	}

	template<typename TExecutor>
	bool Execute(TExecutor *executor, const FlowInboundMap *inbound) {
		int si = 0; // section index
		int bi = 0; // block index
		int ci = 0; // code index

    		int nos = static_cast<int>(shv_->size());
			while (si < nos) {
				const bc::SectionHeader &sh(shv_->at(si++));
				int bib = bi;
				int bie = bi + sh.nob();
				const std::string &id(sh.id());
				const Mounter &mounter(layout_->GetMounter(id));
				int nos = mounter.size();
				for (int k=0;k<nos;k++) { // for each sector
					bi = bib; // reset block index
					int offset = mounter.GetOffset(k);
					while (bi < bie) {
						const bc::BlockHeader &bh(bhv_->at(bi));
						ci = code_offset_[bi++];
						int cib = ci;
						int cie = cib + bh.noc();
						while (ci < cie) {
							const bc::Code &code(cv_->at(ci));
							switch (code.type()) {
							case bc::Code::kBr:
								{
									const bc::Br &br(code.br());
									if (tmp_[br.a()]) {
										ci = cib + br.p();
									} else {
										ci++;
									}
								}
								break;
							case bc::Code::kJmp:
								ci = cib + code.jmp().p();
								break;
							case bc::Code::kCall1:
								DoCall1(code.call1(), tmp_);
								ci++;
								break;
							case bc::Code::kCall2:
								DoCall2(code.call2(), tmp_);
								ci++;
								break;
							case bc::Code::kLb:
								if (executor->Lb(code.lb(), offset)) {
									ci++;
								} else {
									std::cerr << "failed to look back: " << ci << std::endl;
									return false;
								}
								break;
							case bc::Code::kLd:
								if (DoLd(code.ld())) {
									ci++;
								} else {
									std::cerr << "failed to load data: " << ci << std::endl;
									return false;
								}
								break;
							case bc::Code::kGen1:
								DoGen1(code.gen1(), tmp_, rng_);
								ci++;
								break;
							case bc::Code::kGen2:
								DoGen2(code.gen2(), tmp_, rng_);
								ci++;
								break;
							case bc::Code::kLoad:
								if (executor->Load(code.load(), offset)) {
									ci++;
								} else {
									std::cerr << "failed to load: " << ci << std::endl;
									return false;
								}
								break;
							case bc::Code::kLoadi:
								{
									const bc::Loadi &loadi(code.loadi());
									tmp_[loadi.a()] = loadi.v();
									ci++;
								}
								break;
							case bc::Code::kRet:
								ci = cie;
								break;
							case bc::Code::kStore:
								double v = executor->Store(code.store(), offset);
								if (!IsFinite(v, offset)) return false;
								ci++;
								break;
							}
						}
					}
				}
			}

		// in case of empty byte code
		if (nos == 0) return true;
		executor->Communicate(inbound);
		executor->Flush();
		return true;
	}

private:
	void CollectCalculationDependencies(CalculationDependencyVector *cdv) {
		assert(cdv);

		int si = 0; // section index
		int bi = 0; // block index
		int ci = 0; // code index

		int nos = static_cast<int>(shv_->size());
    		for (;si < nos; si++) {
				const bc::SectionHeader &sh(shv_->at(si));
				int bib = bi;
				int bie = bi + sh.nob();
				const std::string &id(sh.id());
				const Mounter &mounter(layout_->GetMounter(id));
				int nos = mounter.size();
				for (int k=0;k<nos;k++) { // for each sector
					bi = bib; // reset block index
					int offset = mounter.GetOffset(k);
					for (;bi < bie; bi++) {
						std::unique_ptr<CalculationDependency> cd(new CalculationDependency(si, k, bi));

						const bc::BlockHeader &bh(bhv_->at(bi));
						ci = code_offset_[bi];
						int cib = ci;
						int cie = cib + bh.noc();
						for (;ci < cie; ci++) {
							const bc::Code &code(cv_->at(ci));
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
							default:
								break;
							}
						}

						cdv->push_back(cd.release());
					}
				}
			}
	}

	void CollectReductionUnits(int nol, const FlowInboundMap *inbound,
							   ReductionUnitVector *ruv)
	{
		assert(inbound);
		assert(ruv);

		for (auto it=inbound->cbegin();it!=inbound->cend();++it) {
			for (int i=0;i<nol;i++) {
				if (i%2 == 0) { // only on an even layer
					std::unique_ptr<ReductionUnit> rd(new ReductionUnit(it->second.first,
																		it->first + (i * layer_size_)));
					for (int src : it->second.second) {
						rd->AddSourceAddr(src + (i * layer_size_));
					}
					ruv->push_back(std::move(rd));
				}
			}
		}
	}

	bool DoLd(const bc::Ld &ld) {
		assert(tv_);
		return tv_->at(ld.i0())->Lookup(ld.i1(), tmp_[ld.d()], tmp_+ld.a());
	}

	const Layout *layout_;
	size_t layer_size_;

	std::unique_ptr<ShVector> shv_;
	std::unique_ptr<BhVector> bhv_;
	std::unique_ptr<CVector> cv_;

	std::vector<ExecutionUnit> euv_;

	std::unique_ptr<int[]> code_offset_;
	double *tmp_;
	TimeseriesVector *tv_;
	std::mt19937 *rng_;
};

}

#endif
