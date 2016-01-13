/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "runtime/section-context.h"

#include <cstdio>
#include <cstring>
#include <iostream>

#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

using std::cerr;
using std::endl;

namespace flint {
namespace runtime {

void ReportSectionContext(const bc::SectionHeader &sh)
{
	boost::uuids::uuid u;
	std::memcpy(&u, sh.id().data(), u.size());
	cerr << " in " << u << endl;
}

}
}
