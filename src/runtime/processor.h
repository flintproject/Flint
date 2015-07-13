/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_PROCESSOR_H_
#define FLINT_RUNTIME_PROCESSOR_H_

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <iterator>
#include <memory>
#include <set>
#include <string>
#include <unordered_set>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/random.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "bc.pb.h"

#include "bc/bc_loader.h"
#include "lo/layout.h"
#include "numeric/prng.h"
#include "runtime/flow.hh"
#include "runtime/timeseries.h"

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
	case bc::Gen2::kUniformVariate:
		tmp[a] = GetUniformVariate(tmp[a1], tmp[a2], rng);
		break;
	}
}

} // namespace

class FlowDependency {
public:
	explicit FlowDependency(int target_addr)
	: target_addr_(target_addr),
	  source_addrs_()
	{}

	int target_addr() const {return target_addr_;}
	const std::unordered_set<int> &source_addrs() const {return source_addrs_;}

	void AddSourceAddr(int source_addr) {
		source_addrs_.insert(source_addr);
	}

private:
	int target_addr_;
	std::unordered_set<int> source_addrs_;
};

typedef boost::ptr_vector<FlowDependency> FlowDependencyVector;

class ExecutionUnit : boost::noncopyable {
public:
	ExecutionUnit(int section_index, int sector_index, int block_index)
		: section_index_(section_index),
		  sector_index_(sector_index),
		  block_index_(block_index),
		  flow_addrs_()
	{}

	int section_index() const {return section_index_;}
	int sector_index() const {return sector_index_;}
	int block_index() const {return block_index_;}
	const std::unordered_set<int> &flow_addrs() const {return flow_addrs_;}

	void Insert(int offset, const std::unordered_set<int> &flow_addrs) {
		for (int a : flow_addrs) {
			flow_addrs_.insert(offset + a);
		}
	}

private:
	int section_index_;
	int sector_index_;
	int block_index_;
	std::unordered_set<int> flow_addrs_;
};

class ExecutionDependency {
public:
	// This does *not* own the given ExecutionUnit
	explicit ExecutionDependency(ExecutionUnit *eu) : eu_(eu), load_addrs_(), store_addr_(-1) {}

	ExecutionUnit *eu() {return eu_;}
	const std::unordered_set<int> &load_addrs() const {return load_addrs_;}
	int store_addr() const {return store_addr_;}

	void AddLoadAddress(int load_addr)
	{
		load_addrs_.insert(load_addr);
	}

	void SetStoreAddr(int store_addr) {
		if (store_addr_ >= 0) {
			std::cerr << "more than one store addr found: " << store_addr_ << std::endl;
		}
		store_addr_ = store_addr;
	}

private:
	ExecutionUnit *eu_;
	std::unordered_set<int> load_addrs_;
	int store_addr_;
};

typedef boost::ptr_vector<ExecutionDependency> ExecutionDependencyVector;
class Processor : boost::noncopyable {
public:
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
	void set_rng(boost::mt19937 *rng) {rng_ = rng;}

	void CalculateCodeOffset() {
		code_offset_.reset(new int[bhv_->size()]);
		int bi = 0;
		int ci = 0;
		for (BhVector::const_iterator it=bhv_->begin();it!=bhv_->end();++it) {
			code_offset_[bi++] = ci;
			ci += it->noc();
		}
	}

	bool SolveLocation() {
		int bi = 0;
		int ci = 0;
		for (ShVector::const_iterator it=shv_->begin();it!=shv_->end();++it) {
			const Locater &locater(layout_->GetLocater(it->id()));
			int bie = bi + it->nob();
			while (bi < bie) {
				const bc::BlockHeader &bh(bhv_->at(bi++));
				int cie = ci + bh.noc();
				while (ci < cie) {
					bc::Code &code(cv_->at(ci++));
					switch (code.type()) {
					case bc::Code::kLb:
						{
							bc::Lb *lb = code.mutable_lb();
							int so;
							if (!locater.Find(lb->v(), &so)) {
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
							if (!locater.Find(load->v(), &so, &lo)) {
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
							if (!locater.Find(store->v(), &so, &lo)) {
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
		return true;
	}

	int GetMaxNumberOfData() {
		int max_nod = 0;
		for (BhVector::const_iterator it=bhv_->begin();it!=bhv_->end();++it) {
			max_nod = std::max(max_nod, it->nod());
		}
		return max_nod;
	}

	// Suppose output and disposition are zero-initialized
	// Note that this destroys the outbound map!
	void SolveConstantDisposition(FlowOutboundMap *outbound,
								  const double *input,
								  double *output,
								  size_t *disposition) const {
		// construct a subgraph of the flow graph
		std::set<int> constants;
		layout_->CollectConstant(1, layer_size_, &constants);
		FlowInboundMap sub_inbound;
		FlowOutboundMap sub_outbound;
		std::set<int> offsets(constants);
		while (!offsets.empty()) {
			std::set<int> neighbors;
			for (std::set<int>::iterator it=offsets.begin();it!=offsets.end();++it) {
				FlowOutboundMap::iterator obit = outbound->find(*it);
				if (obit == outbound->end()) continue;
				std::set<int> diff;
				std::set_difference(obit->second->begin(), obit->second->end(),
									constants.begin(), constants.end(),
									std::inserter(diff, diff.begin()));
				outbound->erase(obit);
				if (diff.empty()) continue;
				for (std::set<int>::const_iterator dit=diff.begin();dit!=diff.end();++dit) {
					sub_inbound[*dit].insert(*it);
					sub_outbound[*it].insert(*dit);
					neighbors.insert(*dit);
				}
			}
			offsets.swap(neighbors);
		}
		// initial output
		for (std::set<int>::const_iterator it=constants.begin();it!=constants.end();++it) {
			output[*it] = input[*it];
			disposition[*it] = 1;
		}
		// propagate values via topological sorting
		offsets.swap(constants);
		for (;;) {
			std::set<int>::iterator it = offsets.begin();
			if (it == offsets.end()) return; // done
			int source = *it;
			offsets.erase(it);

			FlowOutboundMap::iterator sobit = sub_outbound.find(source);
			if (sobit == sub_outbound.end()) continue;
			for (int target : *sobit->second) {
				output[target] += output[source];
				disposition[target] = disposition[source] + 1;
				// remove an (inbound) edge
				size_t i = sub_inbound[target].erase(source);
				assert(i == 1);
				if (sub_inbound[target].empty()) { // no other incoming edges to the target node
					sub_inbound.erase(target);
					offsets.insert(target); // queue
				}
			}
			// remove (outbound) edges
			sub_outbound.erase(sobit);
		}
	}

	bool SolveDependencies(int nol,
						   const FlowInboundMap *inbound,
						   const FlowOutboundMap *outbound,
						   bool constantAvailable) {
		boost::scoped_ptr<ExecutionDependencyVector> edv(new ExecutionDependencyVector);
		CollectExecutionDependencies(edv.get());

		AnnotateFlow(outbound, edv.get());

		boost::scoped_ptr<FlowDependencyVector> fdv(new FlowDependencyVector);
		CollectFlowDependencies(nol, inbound, fdv.get());

		std::unique_ptr<char[]> ready_addresses(new char[nol * layer_size_]());
		if (constantAvailable)
			layout_->MarkConstant(nol, layer_size_, ready_addresses.get());
		while (!edv->empty() || !fdv->empty()) {
			size_t n = 0;

			// Level 2N
			ExecutionDependencyVector::iterator eit = edv->begin();
			while (eit != edv->end()) {
				const std::unordered_set<int> &la = eit->load_addrs();
				if (std::all_of(la.begin(), la.end(),
								[&ready_addresses](int i){return ready_addresses[i] == 1;})) {
					euv_.push_back(eit->eu());
					ready_addresses[eit->store_addr()] = 1;
					n++;
					eit = edv->erase(eit);
				} else {
					++eit;
				}
			}

			// Level 2N+1
			FlowDependencyVector::iterator fit = fdv->begin();
			while (fit != fdv->end()) {
				const std::unordered_set<int> &sa = fit->source_addrs();
				if (std::all_of(sa.begin(), sa.end(),
								[&ready_addresses](int i){return ready_addresses[i] == 1;})) {
					ready_addresses[fit->target_addr()] = 1;
					n++;
					fit = fdv->erase(fit);
				} else {
					++fit;
				}
			}

			if (n == 0) {
				std::cerr << "failed to solve the rest of dependencies: "
					 << edv->size()
					 << "/"
					 << fdv->size()
					 << std::endl;
				for (ExecutionDependencyVector::const_iterator eit=edv->begin();eit!=edv->end();++eit) {
					const std::unordered_set<int> &la = eit->load_addrs();
					for (std::unordered_set<int>::const_iterator lait=la.begin();lait!=la.end();++lait) {
						if (lait == la.begin()) {
							std::cerr << *lait;
						} else {
							std::cerr << "," << *lait;
						}
						// append a question mark if the address is unprepared
						if (ready_addresses[*lait] == 0) {
							std::cerr << "?";
						}
					}
					std::cerr << " -> " << eit->store_addr() << std::endl;
				}
				for (FlowDependencyVector::const_iterator fit=fdv->begin();fit!=fdv->end();++fit) {
					std::cerr << "target_addr: " << fit->target_addr()
							  << " <- ";
					for (std::unordered_set<int>::const_iterator sit=fit->source_addrs().begin();sit!=fit->source_addrs().end();++sit) {
						if (sit == fit->source_addrs().begin()) {
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
		for (boost::ptr_vector<ExecutionUnit>::const_iterator it=euv_.begin();it!=euv_.end();++it) {
			int si = it->section_index();
			int k = it->sector_index();
			int bi = it->block_index();

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
					double v = executor->Store(code.store(), offset, it->flow_addrs());
					if (!IsFinite(v, offset)) {
						boost::uuids::uuid u;
						std::memcpy(&u, sh.id().c_str(), 16);
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
	void CollectExecutionDependencies(ExecutionDependencyVector *edv) {
		assert(edv);

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
						ExecutionUnit *eu = new ExecutionUnit(si, k, bi);
						ExecutionDependency *ed = new ExecutionDependency(eu);

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
											ed->AddLoadAddress(addr);
										}
										break;
									}
								}
								break;
							case bc::Code::kStore:
								{
									const bc::Store &store = code.store();
									int addr = offset + store.so() + (layer_size_ * store.lo());
									ed->SetStoreAddr(addr);
								}
								break;
							default:
								break;
							}
						}

						edv->push_back(ed);
					}
				}
			}
	}

	void CollectFlowDependencies(int nol, const FlowInboundMap *inbound,
								 FlowDependencyVector *fdv)
	{
		assert(inbound);
		assert(fdv);

		for (FlowInboundMap::const_iterator it=inbound->begin();it!=inbound->end();++it) {
			for (int i=0;i<nol;i++) {
				if (i%2 == 0) { // only on an even layer
					FlowDependency *fd = new FlowDependency(it->first + (i * layer_size_));
					for (int src : *it->second) {
						fd->AddSourceAddr(src + (i * layer_size_));
					}
					fdv->push_back(fd);
				}
			}
		}
	}

	void AnnotateFlow(const FlowOutboundMap *outbound,
					  ExecutionDependencyVector *edv) {
		assert(outbound);
		assert(edv);

		for (boost::ptr_vector<ExecutionDependency>::iterator it=edv->begin();it!=edv->end();++it) {
			int store_addr = it->store_addr();
			int local_addr = store_addr % layer_size_;
			int layer_offset = store_addr / layer_size_;
			if (layer_offset%2) continue; // skip an odd layer
			FlowOutboundMap::const_iterator fit = outbound->find(local_addr);
			if (fit == outbound->end()) continue;
			AnnotateFlow(outbound, *fit->second, layer_offset * layer_size_, it->eu());
		}
	}

	void AnnotateFlow(const FlowOutboundMap *outbound, const std::unordered_set<int> &addrs,
					  int offset, ExecutionUnit *eu) {
		eu->Insert(offset, addrs);
		for (int a : addrs) {
			FlowOutboundMap::const_iterator fit = outbound->find(a);
			if (fit == outbound->end()) continue;
			AnnotateFlow(outbound, *fit->second, offset, eu);
		}
	}

	bool DoLd(const bc::Ld &ld) {
		assert(tv_);
		return tv_->at(ld.i0()).Lookup(ld.i1(), tmp_[ld.d()], tmp_+ld.a());
	}

	const Layout *layout_;
	size_t layer_size_;

	boost::scoped_ptr<ShVector> shv_;
	boost::scoped_ptr<BhVector> bhv_;
	boost::scoped_ptr<CVector> cv_;

	boost::ptr_vector<ExecutionUnit> euv_;

	boost::scoped_array<int> code_offset_;
	double *tmp_;
	TimeseriesVector *tv_;
	boost::mt19937 *rng_;
};

#endif
