/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_BC_H_
#define FLINT_BC_H_

#include "bc.pb.h"

#include <memory>
#include <string>
#include <vector>

namespace flint {

typedef std::vector<bc::SectionHeader> ShVector;
typedef std::vector<bc::BlockHeader> BhVector;
typedef std::vector<bc::Code> CVector;

struct Bytecode {
	int nol;
	std::unique_ptr<ShVector> shv;
	std::unique_ptr<BhVector> bhv;
	std::unique_ptr<CVector> cv;

	Bytecode();
};

}

#endif
