/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "mathml.h"

#include <cstring>

namespace flint {
namespace mathml {

namespace {

const char *kScalarConstantElement[] = {
	"eulergamma",
	"exponentiale",
	"pi"
};

const int kNumScalarConstantElement = static_cast<int>(sizeof(kScalarConstantElement)/sizeof(kScalarConstantElement[0]));

}

bool IsScalarConstantElement(const std::string &name)
{
	for (int i=0;i<kNumScalarConstantElement;i++) {
		if (std::strcmp(name.c_str(), kScalarConstantElement[i]) == 0)
			return true;
	}
	return false;
}

}
}
