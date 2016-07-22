/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "load/var.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

#include <boost/uuid/uuid.hpp>

#include "lo.pb.h"

#include "bc/index.h"
#include "bc/pack.h"
#include "db/query.h"

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
	std::vector<std::unique_ptr<lo::Column> > columns;
};

int AddColumn(void *data, int argc, char **argv, char **names)
{
	State *state = static_cast<State *>(data);
	(void)names;
	assert(argc == 14);

	assert(argv[8]);
	assert(argv[9]);
	int col = std::atoi(argv[8]);
	assert(col > 0);
	int row = std::atoi(argv[9]);
	assert(row > 0);
	int size = col * row;

	std::unique_ptr<lo::Column> c(new lo::Column);
	c->set_position(state->pos);
	c->set_col(col);
	c->set_row(row);
	assert(argv[2]);
	c->set_uuid(argv[2], boost::uuids::uuid::static_size()); // sector_id
	c->set_name(argv[4]); // name
	assert(argv[5]);
	assert(std::strlen(argv[5]) == 1);
	char *output_uuid = argv[12];
	char *output_id = argv[13];
	switch (argv[5][0]) { // type
	case 'v':
		if (output_uuid) {
			assert(output_id);
			state->pos += size;
			return 0;
		}
		c->set_type(lo::V);
		break;
	case 'x':
		assert(!output_uuid && !output_id);
		c->set_type(lo::X);
		break;
	default:
		state->pos += size;
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
	state->columns.push_back(std::move(c));
	state->pos += size;
	return 0;
}

bool Process(sqlite3 *db, State *state)
{
	char *em;
	int e;
	e = sqlite3_exec(db,
					 "SELECT l.*, a.output_uuid, a.output_id FROM layout AS l"
					 " LEFT JOIN aliases AS a ON l.sector_id = a.input_uuid AND l.id = a.input_id",
					 &AddColumn, state, &em);
	if (e != SQLITE_OK) {
		if (e != SQLITE_ABORT)
			cerr << "failed to select layout: " << e << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	return true;
}

}

bool Var(sqlite3 *db, const char *output)
{
	if (!CreateTable(db, "aliases",
					 "AS SELECT output_uuid, output_id, input_uuid, input_id FROM reaches"
					 " WHERE reduction = 1"
					 " GROUP BY input_uuid, input_id"
					 " HAVING COUNT(*) = 1"))
		return false;

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
	for (auto it=state.columns.cbegin();it!=state.columns.cend();++it) {
		if (!PackToOstream(**it, &ofs)) {
			cerr << "failed to pack Column" << endl;
			return false;
		}
	}
	ofs.close();

	return true;
}

}
}
