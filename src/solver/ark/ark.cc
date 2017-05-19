/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "solver/ark/ark.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>
#include <vector>

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

#include <arkode/arkode.h>
#include <arkode/arkode_dense.h>
#include <arkode/arkode_direct.h>
#include <nvector/nvector_serial.h>

#include "bc/index.h"
#include "filter/cutter.h"
#include "filter/writer.h"
#include "job.h"
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

bool Ark::Solve(const task::Task &task, const job::Option &option)
{
	bool with_filter = option.filter_file != nullptr;
	FILE *output_fp = option.output_fp;

	/* skeleton: 2. Set problem dimensions */
	if (!SetProblemDimensions())
		return false;

	std::unique_ptr<filter::Writer> writer;
	if (with_filter) {
		filter::Cutter cutter;
		if (!cutter.Load(option.filter_file, layer_size_))
			return false;
		writer.reset(cutter.CreateWriter());
	}

	/* skeleton: 3. Set vector of initial values */
	if (!SetVectorOfInitialValues(option.input_data->data()))
		return false;

	/* skeleton: 4. Create ARKode object */
	arkode_mem_ = ARKodeCreate();
	if (!arkode_mem_) {
		std::cerr << "failed to create ARKode object" << std::endl;
		return false;
	}

	/* skeleton: 5. Initialize ARKode solver */
	int r = ARKodeInit(arkode_mem_, nullptr, ArkRhs, 0.0, y_);
	if (r != ARK_SUCCESS) {
		std::cerr << "failed to initialize ARKode solver: " << r << std::endl;
		return false;
	}

	/* skeleton: 6. Specify integration tolerances */
	r = ARKodeSStolerances(arkode_mem_, 1e-4, 1e-9);
	if (r != ARK_SUCCESS) {
		std::cerr << "failed to specify integration tolerances: " << r << std::endl;
		return false;
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
		return false;
	}

	/* skeleton: 11. Set mass matrix linear solver optional inputs */
	/* skeleton: 12. Specify rootfinding problem */
	/* skeleton: 13. Advance solution in time */
	size_t granularity = task.granularity;
	double output_start_time = task.output_start_time;

	bool with_control = option.control_file != nullptr;
	std::unique_ptr<boost::interprocess::file_mapping> control_fm;
	std::unique_ptr<boost::interprocess::mapped_region> control_region;
	if (with_control) {
		control_fm.reset(new boost::interprocess::file_mapping(option.control_file, boost::interprocess::read_only));
		control_region.reset(new boost::interprocess::mapped_region(*control_fm, boost::interprocess::read_only));
	}
	char control;

	size_t g = (output_start_time == 0) ? 0 : granularity-1;
	realtype tout = 0;
	realtype tret;
	do {
		tout += data_[kIndexDt];
		tout = std::min(tout, data_[kIndexEnd]);

		r = ARKode(arkode_mem_, tout, y_, &tret, ARK_NORMAL);
		if (r != ARK_SUCCESS) {
			std::cerr << "failed to advance step via ARKode: " << r << std::endl;
			return false;
		}

		// copy output to data
		ReadData(tret, y_);
		if (!auxv_->Evaluate(data_.get()))
			return false;

		if (output_start_time <= data_[kIndexTime]) {
			if (granularity <= 1 || ++g == granularity) {
				if (with_filter) {
					if (!writer->Write(data_.get(), output_fp))
						return false;
				} else {
					if (std::fwrite(data_.get(), sizeof(double), layer_size_, output_fp) != static_cast<size_t>(layer_size_)) {
						std::cerr << "failed to write output" << std::endl;
						return false;
					}
				}
				g = 0;
			}
		}

		if (option.progress_address) {
			if (data_[kIndexEnd] <= 0) {
				std::cerr << "non-positive end time: " << data_[kIndexEnd] << std::endl;
				return false;
			}
			char c = static_cast<char>(100 * (data_[kIndexTime] / data_[kIndexEnd]));
			std::memcpy(option.progress_address, &c, 1);
		}

		if (with_control) {
			memcpy(&control, control_region->get_address(), 1);
			if (control == 1) {
				break;
			}
		}
	} while (tret < data_[kIndexEnd]);

	/* skeleton: 14. Get optional outputs */
	return true;
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
