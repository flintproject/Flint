/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_TIMESERIES_H_
#define FLINT_RUNTIME_TIMESERIES_H_

#include <memory>
#include <set>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/mapped_region.hpp>

namespace flint {

class TimeseriesData {
public:
	TimeseriesData(const TimeseriesData &) = delete;
	TimeseriesData &operator=(const TimeseriesData &) = delete;

	explicit TimeseriesData(boost::filesystem::path path);

	bool Load();

	int GetStep(size_t, const char *buf);

	bool Lookup(int i, double t, double *d);

private:
	bool IsValid() const;

	typedef std::set<double> TimestampSet;

	void Store(int i, TimestampSet::iterator it, double *d);
	void Store(int i, TimestampSet::iterator it0, TimestampSet::iterator it1, double t, double *d);

	boost::filesystem::path path_;
	boost::interprocess::mapped_region mr_;
	size_t offset_;
	std::uint32_t step_size_;
	TimestampSet ts_;
};

typedef std::vector<std::unique_ptr<TimeseriesData> > TimeseriesVector;

}

#endif
