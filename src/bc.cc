/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/bc.h"

#include <cstdio>
#include <fstream>
#include <iostream>

#include "bc/pack.h"
#include "flint/ct.h"

namespace flint {

namespace {

class Loader {
public:
	Loader(const Loader &) = delete;
	Loader &operator=(const Loader &) = delete;

	explicit Loader(const std::string &file) : ifs_(file.c_str(), std::ios::in|std::ios::binary) {}

	~Loader() {
		if (ifs_.is_open()) ifs_.close();
	}

	bool Load(int *nol, ct::DataFlowAnalyzer *dfa) {
		if (!ifs_.is_open()) {
			std::cerr << "failed to open bc file" << std::endl;
			return false;
		}

		ct::ShVector *shv = dfa->GetShv();
		ct::BhVector *bhv = dfa->GetBhv();
		ct::CVector *cv = dfa->GetCv();

		if (!UnpackFromIstream(header_, &ifs_)) {
			return false;
		}
		if (nol) *nol = header_.nol();
		int nos = header_.nos();
		shv->resize(nos);
		for (int i=0;i<nos;i++) {
			if (!UnpackFromIstream(shv->at(i), &ifs_)) {
				return false;
			}
		}

		int nob = 0;
		for (const auto &sh : *shv) {
			nob += sh.nob();
		}
		bhv->resize(nob);
		for (int i=0;i<nob;i++) {
			if (!UnpackFromIstream(bhv->at(i), &ifs_)) {
				return false;
			}
		}

		int noc = 0;
		for (const auto &bh : *bhv) {
			noc += bh.noc();
		}
		cv->resize(noc);
		for (int i=0;i<noc;i++) {
			if (!UnpackFromIstream(cv->at(i), &ifs_)) {
				return false;
			}
		}
		return ifs_.peek() == EOF;
	}

private:
	std::ifstream ifs_;
	bc::Header header_;
};

}

bool LoadBytecode(const std::string &file, int *nol, ct::DataFlowAnalyzer *dfa)
{
	Loader loader(file);
	return loader.Load(nol, dfa);
}

}
