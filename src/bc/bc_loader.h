/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_BC_BC_LOADER_H_
#define FLINT_BC_BC_LOADER_H_

#include <fstream>
#include <memory>
#include <string>
#include <vector>

#include "bc/pack.h"

#include "bc.pb.h"

namespace flint {

typedef std::vector<bc::SectionHeader> ShVector;
typedef std::vector<bc::BlockHeader> BhVector;
typedef std::vector<bc::Code> CVector;

class BcLoader {
public:
	BcLoader(const BcLoader &) = delete;
	BcLoader &operator=(const BcLoader &) = delete;

	explicit BcLoader(const std::string &file) : ifs_(file.c_str(), std::ios::in|std::ios::binary) {}

	~BcLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename TProcessor>
	bool Load(int *nol, TProcessor *processor) {
		if (!ifs_.is_open()) {
			std::cerr << "failed to open bc file" << std::endl;
			return false;
		}

		ShVector *shv = processor->GetShv();
		BhVector *bhv = processor->GetBhv();
		CVector *cv = processor->GetCv();

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

#endif
