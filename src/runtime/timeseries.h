/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_TIMESERIES_H_
#define FLINT_RUNTIME_TIMESERIES_H_

#include <cassert>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <set>

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/math/special_functions/fpclassify.hpp>
#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include "isdf/reader.h"

class TimeseriesData : boost::noncopyable {
public:
	explicit TimeseriesData(const char *file)
	: file_(file),
	  fm_(file, boost::interprocess::read_only),
	  offset_(),
	  step_size_(),
	  ts_()
	{}

	~TimeseriesData() {}

	bool Load() {
		std::ifstream ifs(file_, std::ios::in|std::ios::binary);
		if (!ifs.is_open()) {
			std::cerr << "failed to open timeseries data: "
					  << file_
					  << std::endl;
			return false;
		}
		boost::scoped_ptr<isdf::Reader> reader(new isdf::Reader);
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

	int GetStep(size_t, const char *buf) {
		double t;
		std::memcpy(&t, buf, sizeof(t));
		if (!ts_.insert(t).second) {
			std::cerr << "duplicate time step in "
					  << file_
					  << ": "
					  << t
					  << std::endl;
			return -1;
		}
		return 1;
	}

	bool Lookup(int i, double t, double *d) {
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

private:
	typedef std::set<double> TimestampSet;

	void Store(int i, TimestampSet::iterator it, double *d) {
		size_t o = offset_;
		o += step_size_ * std::distance(ts_.begin(), it);
		o += i * sizeof(double);
		boost::interprocess::mapped_region mr(fm_, boost::interprocess::read_only,
											  o, sizeof(*d));
		std::memcpy(d, mr.get_address(), sizeof(*d));
	}

	void Store(int i, TimestampSet::iterator it0, TimestampSet::iterator it1, double t, double *d) {
		double d0, d1;
		Store(i, it0, &d0);
		Store(i, it1, &d1);
		// take the linear interpolation
		double dl = d0 + ((d1-d0)/(*it1-*it0))*(t-*it0);
		if (boost::math::isfinite(dl)) {
			*d = dl;
		} else { // fall back on the nearest neighbor
			if ((t-*it0) < (*it1-t)) {
				*d = d0;
			} else {
				*d = d1;
			}
		}
	}

	const char *file_;
	boost::interprocess::file_mapping fm_;
	size_t offset_;
	boost::uint32_t step_size_;
	TimestampSet ts_;
};

typedef boost::ptr_vector<TimeseriesData> TimeseriesVector;

class TimeseriesLoader : boost::noncopyable {
public:
	explicit TimeseriesLoader(const char *file)	: file_(file) {}

	bool Load(TimeseriesVector *tv) {
		assert(tv);

		// check whether the file is empty or not
		FILE *fp = std::fopen(file_, "rb");
		if (!fp) {
			std::perror(file_);
			return false;
		}
		int r = std::fseek(fp, 0L, SEEK_END);
		if (r < 0) {
			std::perror(file_);
			std::fclose(fp);
			return false;
		}
		long s = std::ftell(fp);
		if (s < 0) {
			std::perror(file_);
			std::fclose(fp);
			return false;
		}
		if (s == 0) { // OK, it's empty. Nothing to do
			std::fclose(fp);
			return true;
		}
		std::fclose(fp);

		boost::interprocess::file_mapping fm(file_, boost::interprocess::read_only);
		boost::interprocess::mapped_region mr(fm, boost::interprocess::read_only);
		char *addr = static_cast<char *>(mr.get_address());
		size_t size = mr.get_size();
		assert(size > 0);
		char *p = addr;
		do {
			tv->push_back(new TimeseriesData(p));
			while (*p++) ;
		} while (p < addr + size);
		for (TimeseriesVector::iterator it=tv->begin();it!=tv->end();++it) {
			if (!it->Load()) return false;
		}
		return true;
	}

private:
	const char *file_;
};

#endif
