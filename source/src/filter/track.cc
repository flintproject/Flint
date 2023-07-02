/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "filter.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem/fstream.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"
#include "ipc.pb.h"

#include "filter/filter_loader.h"

namespace flint {
namespace filter {

namespace {

class Filter {
public:
	Filter(const Filter &) = delete;
	Filter &operator=(const Filter &) = delete;

	explicit Filter(ipc::SimulationTrack *st) : st_(st) {}

	void ReadHeader(int /*size*/) const {
		// ignore header
	}

	void ReadColumn(std::unique_ptr<lo::Column> &&column) {
		if (column->position() == 0) {
			st_->add_key("time");
		} else {
			boost::uuids::uuid u;
			std::memcpy(&u, column->uuid().data(), u.size());
			std::string us = to_string(u);
			st_->add_key(us+":"+column->name());
		}
		st_->add_name(column->name());
		st_->add_scope_name(column->track_name());
		st_->add_label(column->label());
		st_->add_col(column->col());
		st_->add_row(column->row());
	}

private:
	ipc::SimulationTrack *st_;
};

}

bool Track(const boost::filesystem::path &filter_file,
		   const boost::filesystem::path &output_file)
{
	std::unique_ptr<ipc::SimulationTrack> st(new ipc::SimulationTrack);

	// load filter
	std::unique_ptr<Filter> filter(new Filter(st.get()));
	{
		std::unique_ptr<FilterLoader> loader(new FilterLoader(filter_file));
		if (!loader->Load(filter.get())) return false;
	}
	boost::filesystem::ofstream ofs(output_file, std::ios::binary);
	if (!ofs) {
		std::cerr << "failed to open " << output_file << std::endl;
		return false;
	}
	if (!st->SerializeToOstream(&ofs)) {
		std::cerr << "failed to serialize SimulationTrack" << std::endl;
		return false;
	}
	ofs.close();
	return true;
}

}
}
