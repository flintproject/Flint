/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "filter.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem/fstream.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"

#include "filter/filter_loader.h"
#include "isdf/isdf.h"

namespace flint {
namespace filter {

namespace {

class Filter {
public:
	Filter(const Filter &) = delete;
	Filter &operator=(const Filter &) = delete;

	Filter()
		: num_objs_(),
		  num_bytes_descs_(),
		  num_bytes_units_(),
		  descriptions_(),
		  units_()
	{}

	void ReadHeader(int /*size*/) const {
		// ignore header
	}

	void ReadColumn(std::unique_ptr<lo::Column> &&column) {
		boost::uuids::uuid u;
		std::memcpy(&u, column->uuid().data(), u.size());
		std::string s;
		if (u.is_nil()) {
			s = column->name();
		} else {
			s = to_string(u) + ":" + column->name();
			if (column->has_label()) s += "@" + column->label();
		}
		int size = column->col() * column->row();
		num_objs_ += size;
		num_bytes_descs_ += (sizeof(std::uint32_t)+s.size()) * size;
		num_bytes_units_ += (sizeof(std::uint32_t)+column->unit().size()) * size;
		for (int i=0;i<size;i++) {
			descriptions_.push_back(s);
			units_.push_back(column->unit());
		}
	}

	void Write(std::ofstream *ofs) const {
		isdf::ISDFHeader header;
		header.num_objs = num_objs_;
		header.num_bytes_comment = 0;
		header.num_bytes_descs = num_bytes_descs_;
		header.num_bytes_units = num_bytes_units_;

		char buf[sizeof(isdf::ISDFHeader)];
		std::memcpy(buf, &header, sizeof(header));
		ofs->write(buf, sizeof(header));

		for (const auto &desc : descriptions_) {
			std::uint32_t len = desc.size();
			std::memcpy(buf, &len, sizeof(len));
			ofs->write(buf, sizeof(len));
			ofs->write(desc.c_str(), len);
		}
		for (const auto &u : units_) {
			std::uint32_t len = u.size();
			std::memcpy(buf, &len, sizeof(len));
			ofs->write(buf, sizeof(len));
			ofs->write(u.c_str(), len);
		}
	}

private:
	std::uint32_t num_objs_;
	std::uint32_t num_bytes_descs_;
	std::uint32_t num_bytes_units_;
	std::vector<std::string> descriptions_;
	std::vector<std::string> units_;
};

}

bool Isdh(const boost::filesystem::path &filter_file,
		  const boost::filesystem::path &output_file)
{
	std::unique_ptr<Filter> filter(new Filter());
	{
		std::unique_ptr<FilterLoader> loader(new FilterLoader(filter_file));
		if (!loader->Load(filter.get())) return false;
	}
	boost::filesystem::ofstream ofs(output_file, std::ios::out|std::ios::binary);
	if (!ofs.is_open()) {
		std::cerr << "could not open output file: " << output_file << std::endl;
		return false;
	}
	filter->Write(&ofs);
	ofs.close();
	return true;
}

}
}
