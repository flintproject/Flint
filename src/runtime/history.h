/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_HISTORY_H_
#define FLINT_RUNTIME_HISTORY_H_

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <map>

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/noncopyable.hpp>

#include "bc.pb.h"

namespace flint {

class History : boost::noncopyable {
public:
	History() : capacity_(), m_() {}

	void set_capacity(double capacity) {capacity_ = capacity;}

	bool Dump(FILE *fp) const {
		using std::fwrite;

		assert(fp);

		std::uint64_t n = static_cast<std::uint64_t>(m_.size());
		fwrite(&n, sizeof(n), 1, fp);
		for (HistoryMap::const_iterator it=m_.begin();it!=m_.end();++it) {
			fwrite(&it->first, sizeof(double), 1, fp);
			fwrite(&it->second, sizeof(double), 1, fp);
		}
		return true;
	}

	void Insert(double t, double v) {
		HistoryMap::iterator it = m_.begin();
		while (it != m_.end()) {
			if (it->first + capacity_ >= t) {
				break;
			}
			++it;
		}
		m_.erase(m_.begin(), it);
		m_.insert(std::make_pair(t, v));
	}

	char *Load(char *p) {
		using std::memcpy;

		assert(p);

		std::uint64_t n;
		memcpy(&n, p, sizeof(n));
		p += sizeof(n);
		for (std::uint64_t k=0;k<n;k++) {
			double t;
			double v;
			memcpy(&t, p, sizeof(t));
			p += sizeof(t);
			memcpy(&v, p, sizeof(v));
			p += sizeof(v);
			if (!m_.insert(std::make_pair(t, v)).second) {
				std::cerr << "duplicate time: "
						  << t
						  << ": "
						  << v
						  << std::endl;
				return NULL;
			}
		}
		return p;
	}

	bool Lookback(const bc::Lb &lb, double time, double *tmp)
	{
		assert(tmp);

		double t = tmp[lb.d()];
		std::pair<HistoryMap::iterator, HistoryMap::iterator> r;
		r = m_.equal_range(t);
		if (r.first != r.second) { // found the exact one
			tmp[lb.a()] = r.first->second;
			return true;
		}
		if (r.first == m_.begin()) { // older than every existing one
			// check if delay overflows capacity
			if (t < time - capacity_) {
				std::cerr << "failed to look back the value (index "
						  << lb.d()
						  << ", time "
						  << t
						  << "), possibly due to too small value of <max-delay>: "
						  << capacity_
						  << ": "
						  << time
						  << std::endl;
				return false;
			}
			tmp[lb.a()] = r.first->second;
			return true;
		}
		HistoryMap::iterator it = m_.end();
		if (r.first == it) { // later than every existing one
			// take the last
			--it;
			tmp[lb.a()] = it->second;
			return true;
		}
		// take the nearest
		it = r.first;
		--it;
		if ((t - it->first) < (r.first->first - t)) {
			tmp[lb.a()] = it->second;
		} else {
			tmp[lb.a()] = r.first->second;
		}
		return true;
	}

private:
	typedef std::map<double, double> HistoryMap;

	double capacity_;
	HistoryMap m_;
};

class HistoryDumper : boost::noncopyable {
public:
	explicit HistoryDumper(const char *file) : file_(file) {}

	bool Dump(size_t size, const History *history) {
		using std::fclose;

		assert(history);

		FILE *fp = std::fopen(file_, "wb");
		if (!fp) {
			std::perror(file_);
			return false;
		}
		for (size_t i=0;i<size;i++) {
			if (!history[i].Dump(fp)) {
				fclose(fp);
				return false;
			}
		}
		fclose(fp);
		return true;
	}

private:
	const char *file_;
};

class HistoryLoader : boost::noncopyable {
public:
	explicit HistoryLoader(const char *file) : file_(file) {}

	bool Load(size_t size, History *history) {
		assert(history);

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
		size_t bs = mr.get_size();
		assert(size > 0);
		char *p = addr;
		for (size_t i=0;i<size;i++) {
			p = history[i].Load(p);
			if (!p) return false;
		}
		return p == addr + bs;
	}

private:
	const char *file_;
};

}

#endif
