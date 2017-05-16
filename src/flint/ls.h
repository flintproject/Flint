/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LS_H_
#define FLINT_LS_H_

#include <memory>
#include <mutex>
#include <unordered_map>

#include "flint/dps.h"
#include "lo/layout.h"

namespace flint {
namespace ls {

// For the method of least-squares

/*
 * Shared with every job in the task.
 */
struct Configuration {
	Configuration();

	const char *filename;
	size_t data_offset; // unit: in bytes
	size_t row_size;    // unit: in bytes
	std::unordered_map<int, size_t> indice; // the index in datapoints to the index in data
	double bound;
	std::mutex mutex;
};

/* given dps_path is encoded in UTF-8 */
std::unique_ptr<Configuration> CreateConfiguration(const char *dps_path, const Layout &layout);

/*
 * Job (thread) specific.
 */
class Accumulator {
public:
	explicit Accumulator(Configuration &config);

	enum class State {
		kLeq,
		kGt,
		kDone
	};

	/*
	 * Return kLeq if the current value is less than or equals the bound value,
	 * kGt if the current value is greater than the bount value,
	 * kDone otherwise.
	 */
	State operator()(const double *data);

private:
	Configuration &config_;
	dps::Cursor cursor_;
	double sum_;
};

}
}

#endif
