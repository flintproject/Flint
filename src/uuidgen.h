/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_UUIDGEN_H_
#define FLINT_UUIDGEN_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/uuid/uuid_generators.hpp>

namespace flint {

class UuidGenerator {
public:
	UuidGenerator(const UuidGenerator &) = delete;
	UuidGenerator &operator=(const UuidGenerator &) = delete;

	explicit UuidGenerator(const boost::filesystem::path &);

	boost::uuids::uuid operator()();

private:
	boost::mt19937 ran_;
	boost::uuids::basic_random_generator<boost::mt19937> gen_;
};

}

#endif
