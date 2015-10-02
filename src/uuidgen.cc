/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "uuidgen.h"

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

using std::cerr;
using std::endl;

namespace flint {
namespace {

class Adler32Loader {
public:
	Adler32Loader(const Adler32Loader &) = delete;
	Adler32Loader &operator=(const Adler32Loader &) = delete;

	explicit Adler32Loader(const char *file)
  	: fm_(file, boost::interprocess::read_only),
	  mr_(fm_, boost::interprocess::read_only)
	{
	}

	bool Load(std::uint32_t *value) {
		void *addr = mr_.get_address();
		size_t size = mr_.get_size();
		if (size == 0) {
			cerr << "mapped region is of size 0" << endl;
			return false;
		}
		std::uint32_t a = 1;
		std::uint32_t b = 0;
		char *data = static_cast<char *>(addr);
		for (size_t i=0;i<size;i++) {
			a += data[i];
			a %= kMod;
			b += a;
			b %= kMod;
		}
		*value = (b<<16)|a;
		return true;
	}

private:
	static const int kMod = 65521;

	boost::interprocess::file_mapping fm_;
	boost::interprocess::mapped_region mr_;
};

} // namespace

UuidGenerator::UuidGenerator(const boost::filesystem::path &path)
	: ran_(),
	  gen_(&ran_)
{
	std::string path_s = path.string();
	std::uint32_t value;
	std::unique_ptr<Adler32Loader> loader(new Adler32Loader(path_s.c_str()));
	if (!loader->Load(&value)) exit(EXIT_FAILURE);
	ran_.seed(value);
}

boost::uuids::uuid UuidGenerator::operator()()
{
	return gen_();
}

}
