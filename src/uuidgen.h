/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_UUIDGEN_H_
#define FLINT_UUIDGEN_H_

#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/noncopyable.hpp>
#include <boost/uuid/uuid_generators.hpp>

class UuidGenerator : boost::noncopyable {
public:
	explicit UuidGenerator(const boost::filesystem::path &);

	std::string operator()();

private:
	boost::mt19937 ran_;
	boost::uuids::basic_random_generator<boost::mt19937> gen_;
};

#endif
