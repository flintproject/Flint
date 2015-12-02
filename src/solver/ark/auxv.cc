/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "solver/ark/auxv.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>

#include "runtime/processor.h"

using std::cerr;
using std::endl;

namespace flint {
namespace solver {
namespace ark {

class AuxvExecutor {
public:
	AuxvExecutor(const AuxvExecutor &) = delete;
	AuxvExecutor &operator=(const AuxvExecutor &) = delete;

	AuxvExecutor()
		: data_(nullptr)
		, tmp_(nullptr)
	{}

	void set_data(double *data) {data_ = data;}
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
		assert(store.lo() == 0);
		int k = offset + store.so();
		double v = tmp_[store.a()];
		data_[k] = v;
		return v;
	}

	bool Reduce(const ReductionUnit &ru) {
		return ru(data_);
	}

private:
	double *data_;
	double *tmp_;
};

Auxv::Auxv(Processor *processor)
	: processor_(processor)
	, executor_(new AuxvExecutor)
	, tmp_()
{
	int max_nod = processor_->GetMaxNumberOfData();
	tmp_.reset(new double[max_nod]);
	executor_->set_tmp(tmp_.get());
	processor_->set_tmp(tmp_.get());
}

Auxv::~Auxv() = default;

bool Auxv::Evaluate(double *data)
{
	executor_->set_data(data);
	return processor_->Process(executor_.get());
}

}
}
}
