/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "solver/ark/ark.h"

#include <algorithm>
#include <cassert>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>
#include <mutex>
#include <vector>

#include <arkode/arkode.h>
#include <arkode/arkode_dense.h>
#include <arkode/arkode_direct.h>
#include <nvector/nvector_serial.h>

#include "bc/index.h"
#include "filter/writer.h"
#include "flint/ctrl.h"
#include "flint/ls.h"
#include "flint/stats.h"
#include "lo/layout.h"
#include "solver.h"
#include "solver/ark/auxv.h"
#include "solver/ark/mass.h"
#include "solver/ark/rhs.h"
#include "solver/ark/user-supplied.h"
#include "task.h"

namespace flint {
namespace solver {
namespace ark {

Ark::Ark(const Layout *layout, size_t layer_size,
		 Auxv *auxv, Mass *mass, Rhs *rhs)
	: layout_(layout)
	, layer_size_(layer_size)
	, auxv_(auxv)
	, mass_(mass)
	, rhs_(rhs)
	, data_(new double[layer_size * 2]())
	, dim_(0)
	, states_()
	, y_(nullptr)
	, arkode_mem_(nullptr)
{
}

Ark::~Ark()
{
	/* skeleton: 15. Free solver memory */
	if (arkode_mem_)
		ARKodeFree(&arkode_mem_);

	/* skeleton: 16. Deallocate memory for solution vector */
	if (y_)
		N_VDestroy_Serial(y_);
}

Mass *Ark::mass() const
{
	return mass_;
}

Rhs *Ark::rhs() const
{
	return rhs_;
}

double *Ark::data() const
{
	return data_.get();
}

void Ark::ReadTime(realtype t)
{
	data_[kIndexTime] = t;
}

void Ark::ReadData(realtype t, N_Vector y)
{
	data_[kIndexTime] = t;
	long i = 0;
	for (const auto &p : states_) {
		int offset = p.first;
		int size = p.second;
		for (int k=0;k<size;k++) {
			assert(offset + k < layer_size_);
			data_[offset + k] = NV_Ith_S(y, i);
			i++;
		}
	}
	assert(i == dim_);
}

void Ark::WriteData(int lo, N_Vector y)
{
	long i = 0;
	for (const auto &p : states_) {
		int offset = p.first;
		int size = p.second;
		for (int k=0;k<size;k++) {
			assert(offset + k < layer_size_);
			NV_Ith_S(y, i) = data_[offset + k + (layer_size_ * lo)];
			i++;
		}
	}
	assert(i == dim_);
}

job::Result Ark::Solve(const task::Task &task, const job::Option &option)
{
	filter::Writer *writer = task.writer.get();
	char *control_address = task.GetControlAddress(option.id);
	std::ostream *output_stream = option.output_stream;
	auto *arg = option.arg;

	/* skeleton: 2. Set problem dimensions */
	if (!SetProblemDimensions())
		return job::Result::kFailed;

	/* skeleton: 3. Set vector of initial values */
	if (!SetVectorOfInitialValues(option.input_data->data()))
		return job::Result::kFailed;

	/* skeleton: 4. Create ARKode object */
	arkode_mem_ = ARKodeCreate();
	if (!arkode_mem_) {
		std::cerr << "failed to create ARKode object" << std::endl;
		return job::Result::kFailed;
	}

	/* skeleton: 5. Initialize ARKode solver */
	int r = ARKodeInit(arkode_mem_, nullptr, ArkRhs, 0.0, y_);
	if (r != ARK_SUCCESS) {
		std::cerr << "failed to initialize ARKode solver: " << r << std::endl;
		return job::Result::kFailed;
	}

	/* skeleton: 6. Specify integration tolerances */
	r = ARKodeSStolerances(arkode_mem_, 1e-4, 1e-9);
	if (r != ARK_SUCCESS) {
		std::cerr << "failed to specify integration tolerances: " << r << std::endl;
		return job::Result::kFailed;
	}

	/* skeleton: 7. Set optional inputs */
	r = ARKodeSetUserData(arkode_mem_, this);
	assert(r == ARK_SUCCESS);

	/* skeleton: 8. Attach linear solver module */
	r = ARKDense(arkode_mem_, dim_);
	assert(r == ARK_SUCCESS);

	/* skeleton: 9. Set linear solver optional inputs */
	/* skeleton: 10. Attach mass matrix linear solver module */
	r = ARKMassDense(arkode_mem_, dim_, ArkDlsDenseMass);
	if (r != ARKDLS_SUCCESS) {
		std::cerr << "ARKMassDense() failed: " << r << std::endl;
		return job::Result::kFailed;
	}

	/* skeleton: 11. Set mass matrix linear solver optional inputs */
	/* skeleton: 12. Specify rootfinding problem */
	/* skeleton: 13. Advance solution in time */
	size_t granularity = task.granularity;
	double output_start_time = task.output_start_time;

	char *progress_address = task.GetProgressAddress(option.id);

	std::unique_ptr<ls::Accumulator> accum;
	if (task.ls_config) {
		double *rss_address = task.GetRssAddress(option.id);
		assert(rss_address);
		accum.reset(new ls::Accumulator(*task.ls_config, *rss_address));
	}

	size_t g = (output_start_time == 0) ? 0 : granularity-1;

	job::Result result = job::Result::kSucceeded;
	realtype tout = 0;
	realtype tret;
	int num_steps = 0;
	auto rt_start = std::chrono::steady_clock::now();
	do {
		// pause if requested
		if (arg && arg->paused) {
			std::unique_lock<std::mutex> lock(arg->mutex);
			arg->cv.wait(lock, [&arg]{return !arg->paused;});
		}

		tout += data_[kIndexDt];
		tout = std::min(tout, data_[kIndexEnd]);

		r = ARKode(arkode_mem_, tout, y_, &tret, ARK_NORMAL);
		if (r != ARK_SUCCESS) {
			std::cerr << "failed to advance step via ARKode: " << r << std::endl;
			result = job::Result::kFailed;
			break;
		}

		// copy output to data
		ReadData(tret, y_);
		if (!auxv_->Evaluate(data_.get())) {
			result = job::Result::kFailed;
			break;
		}

		if (output_start_time <= data_[kIndexTime]) {
			if (granularity <= 1 || ++g == granularity) {
				if (writer) {
					if (!writer->Write(data_.get(), *output_stream)) {
						result = job::Result::kFailed;
						break;
					}
				} else {
					if (!output_stream->write(reinterpret_cast<const char *>(data_.get()), sizeof(double) * layer_size_)) {
						std::cerr << "failed to write output" << std::endl;
						result = job::Result::kFailed;
						break;
					}
				}
				g = 0;
			}
		}

		if (progress_address) {
			if (data_[kIndexEnd] <= 0) {
				std::cerr << "non-positive end time: " << data_[kIndexEnd] << std::endl;
				result = job::Result::kFailed;
				break;
			}
			*progress_address = static_cast<char>(100 * std::min(1.0, data_[kIndexTime] / data_[kIndexEnd]));
		}

		if (accum) {
			auto state = (*accum)(data_.get());
			if (state == ls::Accumulator::State::kGt) {
				result = job::Result::kCancelled;
				break;
			}
			if (state == ls::Accumulator::State::kDone)
				accum.reset();
		}

		if (control_address && *control_address == 1) {
			result = job::Result::kCancelled;
			break;
		}

		++num_steps;
	} while (tret < data_[kIndexEnd]);

	// Update the bound value, if needed, only if the time evolution is successful
	if (accum && result == job::Result::kSucceeded)
		accum->UpdateBound();

	(void)stats::Record(rt_start, num_steps, option.dir);

	/* skeleton: 14. Get optional outputs */
	return result;
}

bool Ark::SetProblemDimensions()
{
	dim_ = layout_->SelectStates(&states_);
	if (dim_ <= 0) {
		std::cerr << "the number of states is <= 0" << std::endl; // TODO
		return false;
	}
	return true;
}

bool Ark::SetVectorOfInitialValues(const double *input_data)
{
	y_ = N_VNew_Serial(dim_);
	std::memcpy(data_.get(), input_data, layer_size_ * sizeof(double));
	WriteData(0, y_);
	return true;
}

}
}
}
