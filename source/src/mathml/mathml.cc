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

}

bool IsScalarConstantElement(const std::string &name)
{
	for (const char *e : kScalarConstantElement) {
		if (std::strcmp(name.c_str(), e) == 0)
			return true;
	}
	return false;
}

}
}
