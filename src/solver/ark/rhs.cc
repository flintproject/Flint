/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "solver/ark/rhs.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>

#include "runtime/processor.h"

using std::cerr;
using std::endl;

namespace flint {
namespace solver {
namespace ark {

class RhsExecutor {
public:
	RhsExecutor(const RhsExecutor &) = delete;
	RhsExecutor &operator=(const RhsExecutor &) = delete;

	explicit RhsExecutor(int layer_size)
		: layer_size_(layer_size)
		, data_(nullptr)
		, ir_(nullptr)
		, tmp_(nullptr)
	{}

	void set_data(double *data) {data_ = data;}
	void set_ir(intptr_t *ir) {ir_ = ir;}
	void set_tmp(double *tmp) {tmp_ = tmp;}

	bool Lb(const bc::Lb &/*lb*/, int /*offset*/) {
		assert(false);
	}

	bool Load(const bc::Load &load, int offset) {
		switch (load.lo()) {
		case -2:
			tmp_[load.a()] = data_[load.so()];
			break;
		default:
			{
				assert(load.lo() == 0 || load.lo() == -1);
				int k = offset + load.so();
				tmp_[load.a()] = data_[k];
			}
			break;
		}
		return true;
	}

	double Store(const bc::Store &store, int offset) {
		assert(store.lo() < 2);
		int k = offset + store.so() + (layer_size_ * store.lo());
		double v = tmp_[store.a()];
		data_[k] = v;
		return v;
	}

	double *Refer(const bc::Refer &refer, int offset) {
		switch (refer.lo()) {
		case -2:
			return &data_[refer.so()];
		default:
			{
				assert(refer.lo() == 0 || refer.lo() == -1);
				int k = offset + refer.so();
				return &data_[k];
			}
		}
	}

	void Save(const bc::Save &save, int offset) {
		assert(save.lo() < 2);
		int k = offset + save.so() + (layer_size_ * save.lo());
		std::memmove(&data_[k],
					 reinterpret_cast<double *>(ir_[save.i1()]),
					 sizeof(double)*save.k());
	}

	bool Reduce(const ReductionUnit &ru) {
		return ru(data_);
	}

private:
	int layer_size_;
	double *data_;
	intptr_t *ir_;
	double *tmp_;
};

Rhs::Rhs(int layer_size, Processor *processor)
	: processor_(processor)
	, executor_(new RhsExecutor(layer_size))
	, ir_()
	, tmp_()
{
	int max_noir = processor_->GetMaxNoir();
	ir_.reset(new intptr_t[max_noir]);
	executor_->set_ir(ir_.get());
	processor_->set_ir(ir_.get());
	int max_nod = processor_->GetMaxNumberOfData();
	tmp_.reset(new double[max_nod]);
	executor_->set_tmp(tmp_.get());
	processor_->set_tmp(tmp_.get());
}

Rhs::~Rhs() = default;

bool Rhs::Evaluate(double *data)
{
	executor_->set_data(data);
	return processor_->Process(executor_.get());
}

}
}
}
