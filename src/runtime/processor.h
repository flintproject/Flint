/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_PROCESSOR_H_
#define FLINT_RUNTIME_PROCESSOR_H_

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <iterator>
#include <limits>
#include <memory>
#include <random>
#include <string>
#include <vector>

#include <boost/math/special_functions/factorials.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "bc.pb.h"

#include "bc/mounter.h"
#include "flint/ct.h"
#include "lo/layout.h"
#include "numeric/prng.h"
#include "runtime/flow.h"
#include "runtime/matrix.h"
#include "runtime/section-context.h"
#include "runtime/timeseries.h"

#include "execution-unit.h"

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
	case bc::Call1::kFactorial:
		tmp[a] = boost::math::factorial<double>(static_cast<int>(tmp[a1]));
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
		{
			double diff = std::fabs(tmp[a1] - tmp[a2]);
			if (diff == 0) {
				tmp[a] = 1;
				return;
			}
			double aa1 = std::fabs(tmp[a1]);
			double aa2 = std::fabs(tmp[a2]);
			if (aa1 == 0) {
				tmp[a] = aa2 < std::numeric_limits<double>::epsilon();
			} else if (aa2 == 0) {
				tmp[a] = aa1 < std::numeric_limits<double>::epsilon();
			} else {
				double ma = std::fmax(aa1, aa2);
				tmp[a] = diff < std::numeric_limits<double>::epsilon() * ma;
			}
		}
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
	case bc::Call2::kModulo:
		{
			// Obtain the remainder of Euclidean division expressed efficiently
			// in terms of truncated division.
			// See "Division and Modulus for Computer Scientists":
			// http://research.microsoft.com/pubs/151917/divmodnote.pdf
			double r_t = std::fmod(tmp[a1], tmp[a2]);
			if (r_t < 0)
				r_t += std::fabs(tmp[a2]);
			tmp[a] = r_t;
		}
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

class Processor : public ct::DataFlowAnalyzer {
public:
	Processor(const Processor &) = delete;
	Processor &operator=(const Processor &) = delete;

	Processor(const Layout *layout, int layer_size)
		: ct::DataFlowAnalyzer(layout, layer_size)
		, ir_(nullptr)
		, tmp_(nullptr)
		, tv_(nullptr)
		, rng_(nullptr)
	{}

	void set_ir(intptr_t *ir) {ir_ = ir;}
	void set_tmp(double *tmp) {tmp_ = tmp;}
	void set_tv(TimeseriesVector *tv) {tv_ = tv;}
	void set_rng(std::mt19937 *rng) {rng_ = rng;}

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
				heap_.clear();
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
							runtime::ReportSectionContext(sh);
							return false;
						}
						break;
					case bc::Code::kLd:
						if (DoLd(code.ld())) {
							ci++;
						} else {
							std::cerr << "failed to load data: " << ci << std::endl;
							runtime::ReportSectionContext(sh);
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
							runtime::ReportSectionContext(sh);
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
						{
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
						}
						break;
					case bc::Code::kRefer:
						{
							const bc::Refer &refer(code.refer());
							ir_[refer.i0()] = reinterpret_cast<intptr_t>(executor->Refer(refer, offset));
							ci++;
						}
						break;
					case bc::Code::kDeref:
						{
							const bc::Deref &deref(code.deref());
							tmp_[deref.f0()] = *(reinterpret_cast<double *>(ir_[deref.i1()]) + deref.k());
							ci++;
						}
						break;
					case bc::Code::kAlloc:
						{
							const bc::Alloc &a(code.alloc());
							heap_.emplace_back(new double[a.k()]);
							ir_[a.i0()] = reinterpret_cast<intptr_t>(heap_.back().get());
							ci++;
						}
						break;
					case bc::Code::kSave:
						{
							const bc::Save &save(code.save());
							executor->Save(save, offset);
							ci++;
						}
						break;
					case bc::Code::kMove:
						{
							const bc::Move &move(code.move());
							*(reinterpret_cast<double *>(ir_[move.i0()]) + move.k()) = tmp_[move.f1()];
							ci++;
						}
						break;
					case bc::Code::kTranspose:
						{
							const bc::Transpose &transpose(code.transpose());
							heap_.emplace_back(new double[transpose.kc() * transpose.kr()]);
							double *d0 = heap_.back().get();
							ir_[transpose.i0()] = reinterpret_cast<intptr_t>(d0);
							const double *d1 = reinterpret_cast<const double *>(ir_[transpose.i1()]);
							runtime::Transpose(d0, d1, transpose.kr(), transpose.kc());
							ci++;
						}
						break;
					case bc::Code::kOuterproduct:
						{
							const bc::Outerproduct &outerproduct(code.outerproduct());
							heap_.emplace_back(new double[outerproduct.k1() * outerproduct.k2()]);
							double *d0 = heap_.back().get();
							ir_[outerproduct.i0()] = reinterpret_cast<intptr_t>(d0);
							const double *d1 = reinterpret_cast<const double *>(ir_[outerproduct.i1()]);
							const double *d2 = reinterpret_cast<const double *>(ir_[outerproduct.i2()]);
							runtime::Outerproduct(d0, outerproduct.k1(), d1, outerproduct.k2(), d2);
							ci++;
						}
						break;
					case bc::Code::kScalarproduct:
						{
							const bc::Scalarproduct &scalarproduct(code.scalarproduct());
							const double *d1 = reinterpret_cast<const double *>(ir_[scalarproduct.i1()]);
							const double *d2 = reinterpret_cast<const double *>(ir_[scalarproduct.i2()]);
							tmp_[scalarproduct.f0()] = runtime::Scalarproduct(scalarproduct.k(), d1, d2);
							ci++;
						}
						break;
					case bc::Code::kVectorproduct:
						{
							const bc::Vectorproduct &vectorproduct(code.vectorproduct());
							heap_.emplace_back(new double[3]);
							double *d0 = heap_.back().get();
							ir_[vectorproduct.i0()] = reinterpret_cast<intptr_t>(d0);
							const double *d1 = reinterpret_cast<const double *>(ir_[vectorproduct.i1()]);
							const double *d2 = reinterpret_cast<const double *>(ir_[vectorproduct.i2()]);
							runtime::Vectorproduct(d0, d1, d2);
							ci++;
						}
						break;
					case bc::Code::kDeterminant:
						{
							const bc::Determinant &determinant(code.determinant());
							const double *d1 = reinterpret_cast<const double *>(ir_[determinant.i1()]);
							tmp_[determinant.f0()] = runtime::Determinant(determinant.k(), d1);
							ci++;
						}
						break;
					case bc::Code::kSelect2:
						{
							const bc::Select2 &select2(code.select2());
							const double *d1 = reinterpret_cast<const double *>(ir_[select2.i1()]);
							tmp_[select2.f0()] = runtime::Select2(d1, static_cast<int>(tmp_[select2.f2()]));
							ci++;
						}
						break;
					case bc::Code::kSelect3:
						{
							const bc::Select3 &select3(code.select3());
							const double *d1 = reinterpret_cast<const double *>(ir_[select3.i1()]);
							tmp_[select3.f0()] = runtime::Select3(select3.kr(), select3.kc(), d1,
																  static_cast<int>(tmp_[select3.f2()]),
																  static_cast<int>(tmp_[select3.f3()]));
							ci++;
						}
						break;
					case bc::Code::kSelrow:
						{
							const bc::Selrow &selrow(code.selrow());
							heap_.emplace_back(new double[selrow.kr()]);
							double *d0 = heap_.back().get();
							const double *d1 = reinterpret_cast<const double *>(ir_[selrow.i1()]);
							runtime::Selrow(d0, selrow.kr(), selrow.kc(), d1, static_cast<int>(tmp_[selrow.f2()]));
							ci++;
						}
						break;
					case bc::Code::kMult:
						{
							const bc::Mult &mult(code.mult());
							heap_.emplace_back(new double[mult.k()]);
							double *d0 = heap_.back().get();
							ir_[mult.i0()] = reinterpret_cast<intptr_t>(d0);
							const double *d2 = reinterpret_cast<const double *>(ir_[mult.i2()]);
							runtime::Mult(d0, mult.k(), tmp_[mult.f1()], d2);
							ci++;
						}
						break;
					case bc::Code::kMmul:
						{
							const bc::Mmul &mmul(code.mmul());
							heap_.emplace_back(new double[mmul.kc() * mmul.kr()]);
							double *d0 = heap_.back().get();
							ir_[mmul.i0()] = reinterpret_cast<intptr_t>(d0);
							const double *d1 = reinterpret_cast<const double *>(ir_[mmul.i1()]);
							const double *d2 = reinterpret_cast<const double *>(ir_[mmul.i2()]);
							runtime::Mmul(d0, mmul.kr(), mmul.kx(), mmul.kc(), d1, d2);
							ci++;
						}
						break;
					default:
						assert(false);
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
					heap_.clear();
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
								runtime::ReportSectionContext(sh);
								return false;
							}
							break;
						case bc::Code::kLd:
							if (DoLd(code.ld())) {
								ci++;
							} else {
								std::cerr << "failed to load data: " << ci << std::endl;
								runtime::ReportSectionContext(sh);
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
								runtime::ReportSectionContext(sh);
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
							{
								double v = executor->Store(code.store(), offset);
								if (!IsFinite(v, offset)) return false;
								ci++;
							}
							break;
						case bc::Code::kRefer:
							{
								const bc::Refer &refer(code.refer());
								ir_[refer.i0()] = reinterpret_cast<intptr_t>(executor->Refer(refer, offset));
								ci++;
							}
							break;
						case bc::Code::kDeref:
							{
								const bc::Deref &deref(code.deref());
								tmp_[deref.f0()] = *(reinterpret_cast<double *>(ir_[deref.i1()]) + deref.k());
								ci++;
							}
							break;
						case bc::Code::kAlloc:
							{
								const bc::Alloc &a(code.alloc());
								heap_.emplace_back(new double[a.k()]);
								ir_[a.i0()] = reinterpret_cast<intptr_t>(heap_.back().get());
								ci++;
							}
							break;
						case bc::Code::kSave:
							{
								const bc::Save &save(code.save());
								executor->Save(save, offset);
								ci++;
							}
							break;
						case bc::Code::kMove:
							{
								const bc::Move &move(code.move());
								*(reinterpret_cast<double *>(ir_[move.i0()]) + move.k()) = tmp_[move.f1()];
								ci++;
							}
							break;
						case bc::Code::kTranspose:
							{
								const bc::Transpose &transpose(code.transpose());
								heap_.emplace_back(new double[transpose.kc() * transpose.kr()]);
								double *d0 = heap_.back().get();
								ir_[transpose.i0()] = reinterpret_cast<intptr_t>(d0);
								const double *d1 = reinterpret_cast<const double *>(ir_[transpose.i1()]);
								runtime::Transpose(d0, d1, transpose.kr(), transpose.kc());
								ci++;
							}
							break;
						case bc::Code::kOuterproduct:
							{
								const bc::Outerproduct &outerproduct(code.outerproduct());
								heap_.emplace_back(new double[outerproduct.k1() * outerproduct.k2()]);
								double *d0 = heap_.back().get();
								ir_[outerproduct.i0()] = reinterpret_cast<intptr_t>(d0);
								const double *d1 = reinterpret_cast<const double *>(ir_[outerproduct.i1()]);
								const double *d2 = reinterpret_cast<const double *>(ir_[outerproduct.i2()]);
								runtime::Outerproduct(d0, outerproduct.k1(), d1, outerproduct.k2(), d2);
								ci++;
							}
							break;
						case bc::Code::kScalarproduct:
							{
								const bc::Scalarproduct &scalarproduct(code.scalarproduct());
								const double *d1 = reinterpret_cast<const double *>(ir_[scalarproduct.i1()]);
								const double *d2 = reinterpret_cast<const double *>(ir_[scalarproduct.i2()]);
								tmp_[scalarproduct.f0()] = runtime::Scalarproduct(scalarproduct.k(), d1, d2);
								ci++;
							}
							break;
						case bc::Code::kVectorproduct:
							{
								const bc::Vectorproduct &vectorproduct(code.vectorproduct());
								heap_.emplace_back(new double[3]);
								double *d0 = heap_.back().get();
								ir_[vectorproduct.i0()] = reinterpret_cast<intptr_t>(d0);
								const double *d1 = reinterpret_cast<const double *>(ir_[vectorproduct.i1()]);
								const double *d2 = reinterpret_cast<const double *>(ir_[vectorproduct.i2()]);
								runtime::Vectorproduct(d0, d1, d2);
								ci++;
							}
							break;
						case bc::Code::kDeterminant:
							{
								const bc::Determinant &determinant(code.determinant());
								const double *d1 = reinterpret_cast<const double *>(ir_[determinant.i1()]);
								tmp_[determinant.f0()] = runtime::Determinant(determinant.k(), d1);
								ci++;
							}
							break;
						case bc::Code::kSelect2:
							{
								const bc::Select2 &select2(code.select2());
								const double *d1 = reinterpret_cast<const double *>(ir_[select2.i1()]);
								tmp_[select2.f0()] = runtime::Select2(d1, static_cast<int>(tmp_[select2.f2()]));
								ci++;
							}
							break;
						case bc::Code::kSelect3:
							{
								const bc::Select3 &select3(code.select3());
								const double *d1 = reinterpret_cast<const double *>(ir_[select3.i1()]);
								tmp_[select3.f0()] = runtime::Select3(select3.kr(), select3.kc(), d1,
																	  static_cast<int>(tmp_[select3.f2()]),
																	  static_cast<int>(tmp_[select3.f3()]));
								ci++;
							}
							break;
						case bc::Code::kSelrow:
							{
								const bc::Selrow &selrow(code.selrow());
								heap_.emplace_back(new double[selrow.kr()]);
								double *d0 = heap_.back().get();
								const double *d1 = reinterpret_cast<const double *>(ir_[selrow.i1()]);
								runtime::Selrow(d0, selrow.kr(), selrow.kc(), d1, static_cast<int>(tmp_[selrow.f2()]));
								ci++;
							}
							break;
						case bc::Code::kMult:
							{
								const bc::Mult &mult(code.mult());
								heap_.emplace_back(new double[mult.k()]);
								double *d0 = heap_.back().get();
								ir_[mult.i0()] = reinterpret_cast<intptr_t>(d0);
								const double *d2 = reinterpret_cast<const double *>(ir_[mult.i2()]);
								runtime::Mult(d0, mult.k(), tmp_[mult.f1()], d2);
								ci++;
							}
							break;
						case bc::Code::kMmul:
							{
								const bc::Mmul &mmul(code.mmul());
								heap_.emplace_back(new double[mmul.kc() * mmul.kr()]);
								double *d0 = heap_.back().get();
								ir_[mmul.i0()] = reinterpret_cast<intptr_t>(d0);
								const double *d1 = reinterpret_cast<const double *>(ir_[mmul.i1()]);
								const double *d2 = reinterpret_cast<const double *>(ir_[mmul.i2()]);
								runtime::Mmul(d0, mmul.kr(), mmul.kx(), mmul.kc(), d1, d2);
								ci++;
							}
							break;
						default:
							assert(false);
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
	bool DoLd(const bc::Ld &ld) {
		assert(tv_);
		return tv_->at(ld.i0())->Lookup(ld.i1(), tmp_[ld.d()], tmp_+ld.a());
	}

	intptr_t *ir_;
	double *tmp_;
	TimeseriesVector *tv_;
	std::mt19937 *rng_;
	std::vector<std::unique_ptr<double[]> > heap_;
};

}

#endif
