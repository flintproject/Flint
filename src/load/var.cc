/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "load/var.hh"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>

#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/uuid/uuid.hpp>

#include "lo.pb.h"

#include "bc/index.h"
#include "bc/pack.h"

using std::cerr;
using std::endl;

namespace flint {
namespace load {

namespace {

struct State {
	State()
		: pos(kOffsetBase)
	{}

	int pos;
	boost::ptr_vector<lo::Column> columns;
};

int AddColumn(void *data, int argc, char **argv, char **names)
{
	State *state = static_cast<State *>(data);
	(void)names;
	assert(argc == 9);
	std::unique_ptr<lo::Column> c(new lo::Column);
	c->set_position(state->pos);
	c->set_size(1); // TODO: variable size
	assert(argv[2]);
	c->set_uuid(argv[2], boost::uuids::uuid::static_size()); // sector_id
	c->set_name(argv[4]); // name
	assert(argv[5]);
	assert(std::strlen(argv[5]) == 1);
	switch (argv[5][0]) { // type
	case 'v':
		c->set_type(lo::V);
		break;
	case 'x':
		c->set_type(lo::X);
		break;
	default:
		state->pos += 1; // TODO: variable size
		return 0;
	}
	c->set_id(std::atoi(argv[6])); // id
	c->set_unit(argv[7]); // unit
	if (argv[1]) {
		c->set_track_name(argv[1]);
	}
	if (argv[3]) { // label
		c->set_label(argv[3]);
	}
	state->columns.push_back(c.release());
	state->pos += 1; // TODO: variable size
	return 0;
}

bool Process(sqlite3 *db, State *state)
{
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT * FROM layout",
					 &AddColumn, state, &em);
	if (e != SQLITE_OK) {
		cerr << "failed to select layout: " << e
			 << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	return true;
}

}

bool Var(sqlite3 *db, const char *output)
{
	State state;
	if (!Process(db, &state))
		return false;
	lo::Header header;
	header.set_size(state.pos);

	std::ofstream ofs(output, std::ios::out|std::ios::binary);
	if (!ofs) {
		cerr << "failed to open " << output << endl;
		return false;
	}
	if (!PackToOstream(header, &ofs)) {
		cerr << "failed to pack Header" << endl;
		return false;
	}
	for (boost::ptr_vector<lo::Column>::const_iterator it=state.columns.begin();it!=state.columns.end();++it) {
		if (!PackToOstream(*it, &ofs)) {
			cerr << "failed to pack Column" << endl;
			return false;
		}
	}
	ofs.close();

	return true;
}

}
}
