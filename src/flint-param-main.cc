/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"

#include "bc/index.h"
#include "db/read-only-driver.hh"
#include "lo/layout_loader.h"
#include "sqlite3.h"

using std::cerr;
using std::cout;
using std::endl;
using std::strcmp;

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
	static boost::uuids::string_generator gen;

	State *state = static_cast<State *>(data);
	(void)names;
	assert(argc == 9);
	std::auto_ptr<lo::Column> c(new lo::Column);
	c->set_position(state->pos);
	c->set_size(1); // TODO: variable size
	boost::uuids::uuid u = gen(argv[2]);
	c->set_uuid(boost::uuids::to_string(u)); // sector_id
	c->set_name(argv[4]); // name
	assert(argv[5]);
	assert(std::strlen(argv[5]) == 1);
	switch (argv[5][0]) { // type
	case 's':
		c->set_type(lo::S);
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

void Usage()
{
	cerr << "usage: flint-param INPUT OUTPUT" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	if (argc == 2) {
		Usage();
		if ( strcmp(argv[1], "-h") == 0 ||
			 strcmp(argv[1], "--help") == 0 )
			return EXIT_SUCCESS;
		return EXIT_FAILURE;
	}
	if (argc != 3) {
		Usage();
		return EXIT_FAILURE;
	}

	db::ReadOnlyDriver driver(argv[1]);
	State state;
	if (!Process(driver.db(), &state))
		return EXIT_FAILURE;
	lo::Header header;
	header.set_size(state.pos);

	std::ofstream ofs(argv[2], std::ios::out|std::ios::binary);
	if (!ofs) {
		cerr << "failed to open " << argv[2] << endl;
		return EXIT_FAILURE;
	}
	if (!PackToOstream(header, &ofs)) {
		cerr << "failed to pack Header" << endl;
		return EXIT_FAILURE;
	}
	for (boost::ptr_vector<lo::Column>::const_iterator it=state.columns.begin();it!=state.columns.end();++it) {
		if (!PackToOstream(*it, &ofs)) {
			cerr << "failed to pack Column" << endl;
			return EXIT_FAILURE;
		}
	}
	ofs.close();

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
