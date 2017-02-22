/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "runtime/timeseries.h"

#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <memory>
#include <set>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem/fstream.hpp>
#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/mapped_region.hpp>

#include "isdf/reader.h"

namespace flint {

TimeseriesData::TimeseriesData(boost::filesystem::path path)
	: path_(path)
{
	std::string path_s = path_.string();
	fm_ = boost::interprocess::file_mapping(path_s.c_str(), boost::interprocess::read_only);
}

bool TimeseriesData::Load()
{
	boost::filesystem::ifstream ifs(path_, std::ios::in|std::ios::binary);
	if (!ifs.is_open()) {
		std::cerr << "failed to open timeseries data: "
				  << path_
				  << std::endl;
		return false;
	}
	std::unique_ptr<isdf::Reader> reader(new isdf::Reader);
	if (!reader->ReadHeader(&ifs)) return false;
	offset_ = reader->GetDataOffset();
	step_size_ = reader->num_objs()*sizeof(double);
	if (!reader->SkipComment(&ifs)) return false;
	if (!reader->SkipDescriptions(&ifs)) return false;
	if (!reader->SkipUnits(&ifs)) return false; /* TODO */
	if (!reader->ReadSteps(*this, &ifs)) return false;
	ifs.close();
	return true;
}

int TimeseriesData::GetStep(size_t, const char *buf)
{
	double t;
	std::memcpy(&t, buf, sizeof(t));
	if (!ts_.insert(t).second) {
		std::cerr << "duplicate time step in "
				  << path_
				  << ": "
				  << t
				  << std::endl;
		return -1;
	}
	return 1;
}

bool TimeseriesData::Lookup(int i, double t, double *d)
{
	std::pair<TimestampSet::iterator, TimestampSet::iterator> r;
	r = ts_.equal_range(t);
	if (r.first != r.second) { // found the exact data
		Store(i, r.first, d);
		return true;
	}
	if (r.first == ts_.begin()) { // smaller than every existing data
		// take the first
		Store(i, r.first, d);
		return true;
	}
	if (r.first == ts_.end()) { // larger than every existing data
		// take the last
		TimestampSet::iterator it = ts_.end();
		--it;
		Store(i, it, d);
		return true;
	}
	TimestampSet::iterator it = r.first;
	--it;
	Store(i, it, r.first, t, d);
	return true;
}

void TimeseriesData::Store(int i, TimestampSet::iterator it, double *d)
{
	size_t o = offset_;
	o += step_size_ * std::distance(ts_.begin(), it);
	o += i * sizeof(double);
	boost::interprocess::mapped_region mr(fm_, boost::interprocess::read_only,
										  o, sizeof(*d));
	std::memcpy(d, mr.get_address(), sizeof(*d));
}

void TimeseriesData::Store(int i, TimestampSet::iterator it0, TimestampSet::iterator it1, double t, double *d)
{
	double d0, d1;
	Store(i, it0, &d0);
	Store(i, it1, &d1);
	// take the linear interpolation
	double dl = d0 + ((d1-d0)/(*it1-*it0))*(t-*it0);
	if (std::isfinite(dl)) {
		*d = dl;
	} else { // fall back on the nearest neighbor
		if ((t-*it0) < (*it1-t)) {
			*d = d0;
		} else {
			*d = d1;
		}
	}
}

}
