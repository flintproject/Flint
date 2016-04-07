/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_HISTORY_H_
#define FLINT_RUNTIME_HISTORY_H_

#include <cstdio>
#include <map>

#include "bc.pb.h"

namespace flint {

class History {
public:
	History(const History &) = delete;
	History &operator=(const History &) = delete;

	History();

	void set_capacity(double capacity) {capacity_ = capacity;}

	bool Dump(FILE *fp) const;

	void Insert(double t, double v);

	char *Load(char *p);

	bool Lookback(const bc::Lb &lb, double time, double *tmp);

private:
	typedef std::map<double, double> HistoryMap;

	double capacity_;
	HistoryMap m_;
};

class HistoryDumper {
public:
	HistoryDumper(const HistoryDumper &) = delete;
	HistoryDumper &operator=(const HistoryDumper &) = delete;

	explicit HistoryDumper(const char *file);

	bool Dump(size_t size, const History *history);

private:
	const char *file_;
};

class HistoryLoader {
public:
	HistoryLoader(const HistoryLoader &) = delete;
	HistoryLoader &operator=(const HistoryLoader &) = delete;

	explicit HistoryLoader(const char *file);

	bool Load(size_t size, History *history);

private:
	const char *file_;
};

}

#endif
