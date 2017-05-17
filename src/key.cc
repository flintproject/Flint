/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/key.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <memory>

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace flint {
namespace key {

bool Data::operator<(const Data &other) const
{
	if (uuid < other.uuid)
		return true;
	if (uuid == other.uuid)
		return name < other.name;
	return false;
}

std::string Data::GetReadableString() const
{
	size_t len = 36+1+name.size();
	std::unique_ptr<char[]> buf(new char[len+1]);
	std::sprintf(buf.get(), "%s:%s", boost::uuids::to_string(uuid).c_str(), name.c_str());
	return std::string(buf.get(), len);
}

std::string Data::GetPrefixString() const
{
	const int kPrefixSize = 48;
	char prefix[kPrefixSize];
	std::memset(prefix, 0, kPrefixSize);
	std::memcpy(prefix, &uuid, uuid.size());
	std::memcpy(prefix+uuid.size(), name.c_str(), name.size());
	return std::string(prefix, kPrefixSize);
}

bool Data::FromString(const std::string &s, Data *d)
{
	if (s.size() > 36+1+32) // too long name
		return false;
	boost::uuids::string_generator gen;
	d->uuid = gen(s.substr(0, 36));
	d->name = s.substr(37);
	return true;
}

}
}
