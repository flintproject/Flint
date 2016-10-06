/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "runtime/section-context.h"

#include <cstdio>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace flint {
namespace runtime {

std::string GetCanonicalName(const std::string &v)
{
	assert(v.size() > 1);
	// trim leading '%' and the tail starting with '#'
	size_t pos = v.find_first_of("#");
	return v.substr(1, (pos == std::string::npos) ? pos : pos-1);
}

void ReportSectionContext(const bc::SectionHeader &sh)
{
	boost::uuids::uuid u;
	std::memcpy(&u, sh.id().data(), u.size());
	std::cerr << " in " << u << std::endl;
}

}
}
