/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "fppp.h"

#include <cstring>

#include <boost/uuid/uuid_generators.hpp>

namespace flint {
namespace fppp {

namespace {

const char kPrefix[] = "fppp:///";

}

bool IsValidUrl(const char *url)
{
	return std::strncmp(url, kPrefix, sizeof(kPrefix)-1) == 0;
}

boost::uuids::uuid GetUuidFromUrl(const char *url)
{
	boost::uuids::string_generator gen;
	return gen(url+(sizeof(kPrefix)-1));
}

}
}
