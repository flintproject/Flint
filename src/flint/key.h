/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_KEY_H_
#define FLINT_KEY_H_

#include <string>

#include <boost/uuid/uuid.hpp>

namespace flint {
namespace key {

struct Data
{
	boost::uuids::uuid uuid;
	std::string name;

	bool operator<(const Data &other) const;

	std::string GetReadableString() const;

	std::string GetPrefixString() const;

	static bool FromString(const std::string &s, Data *d);
};

}
}

#endif
