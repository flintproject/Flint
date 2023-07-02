/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "solver/ark/mass.h"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>

#include "runtime/processor.h"
#include "solver/ark/mmdm.h"

namespace flint {
namespace solver {
namespace ark {

class MassExecutor {
public:
	MassExecutor(const MassExecutor &) = delete;
	MassExecutor &operator=(const MassExecutor &) = delete;

	MassExecutor()
		: data_(nullptr)
		, ir_(nullptr)
		, tmp_(nullptr)
	{}

	void set_data(double *data) {data_ = data;}
	void set_ir(intptr_t *ir) {ir_ = ir;}
	void set_tmp(double *tmp) {tmp_ = tmp;}

	bool Lb(const bc::Lb &/*lb*/, int /*offset*/) {
		assert(false);
		return false;
	}

	bool Load(const bc::Load &load, int offset) {
		switch (load.lo()) {
		case -1:
			{
				int k = offset + load.so();
				tmp_[load.a()] = data_[k];
			}
			break;
		case -2:
			tmp_[load.a()] = data_[load.so()];
			break;
		default:
			{
				assert(load.lo() == 0);
				int k = offset + load.so();
				tmp_[load.a()] = data_[k];
			}
			break;
		}
		return true;
	}

	double Store(const bc::Store &store, int offset) {
		assert(store.lo() == 0);
		int k = offset + store.so();
		double v = tmp_[store.a()];
		data_[k] = v;
		return v;
	}

	double *Refer(const bc::Refer &refer, int offset) {
		switch (refer.lo()) {
		case -1:
			{
				int k = offset + refer.so();
				return &data_[k];
			}
		case -2:
			return &data_[refer.so()];
		default:
			{
				assert(refer.lo() == 0);
				int k = offset + refer.so();
				return &data_[k];
			}
		}
	}

	void Save(const bc::Save &save, int offset) {
		assert(save.lo() == 0);
		int k = offset + save.so();
		std::memmove(&data_[k],
					 reinterpret_cast<double *>(ir_[save.i1()]),
					 sizeof(double)*save.k());
	}

	bool Reduce(const ReductionUnit &ru) {
		return ru(data_);
	}

private:
	double *data_;
	intptr_t *ir_;
	double *tmp_;
};

Mass::Mass(Processor *processor, Mmdm *mmdm)
	: processor_(processor)
	, mmdm_(mmdm)
	, executor_(new MassExecutor)
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

Mass::~Mass() = default;

bool Mass::Evaluate(double *data)
{
	executor_->set_data(data);
	return processor_->Process(executor_.get());
}

void Mass::Write(const double *data, DlsMat M)
{
	// Suppose that M is initialized to the zero matrix
	// column-major
	for (long col=0;col<mmdm_->size();col++) {
		for (long row=0;row<mmdm_->size();row++) {
			long i = mmdm_->Find(row, col);
			switch (i) {
			case 0:
				// nothing to do
				break;
			case 1:
				DENSE_ELEM(M, row, col) = 1;
				break;
			default:
				DENSE_ELEM(M, row, col) = data[i];
				break;
			}
		}
	}
}

}
}
}
