/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DPS_H_
#define FLINT_DPS_H_

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

namespace flint {
namespace dps {

// Datapoints: an alternative data structure of timeseries data

class Cursor {
public:
	Cursor(boost::interprocess::file_mapping fm, size_t offset, size_t row_size);

	enum class Position {
		kGt,
		kLeq,
		kEnd
	};

	/*
	 * Return kGt if the time of current point is greater than given t,
	 * kLeq if it is less than or equals to given t,
	 * kEnd otherwise.
	 */
	Position operator()(double t, double **p);

private:
	boost::interprocess::mapped_region mr_;
	size_t row_size_;
	size_t position_;
};

}
}

#endif
