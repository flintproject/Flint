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

#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"
#include "ipc.pb.h"

#include "filter/filter_loader.h"

using std::cerr;
using std::endl;
using std::memcpy;
using std::string;

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
			memcpy(&u, column->uuid().data(), u.size());
			string us = to_string(u);
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

bool Track(const char *filter_file, const char *output_file)
{
	std::unique_ptr<ipc::SimulationTrack> st(new ipc::SimulationTrack);

	// load filter
	std::unique_ptr<Filter> filter(new Filter(st.get()));
	{
		std::unique_ptr<FilterLoader> loader(new FilterLoader(filter_file));
		if (!loader->Load(filter.get())) return false;
	}
	std::ofstream ofs(output_file, std::ios::binary);
	if (!ofs) {
		cerr << "failed to open " << output_file << endl;
		return false;
	}
	if (!st->SerializeToOstream(&ofs)) {
		cerr << "failed to serialize SimulationTrack" << endl;
		return false;
	}
	ofs.close();
	return true;
}

}
}
