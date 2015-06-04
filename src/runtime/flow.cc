/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flow.hh"

#include <cstdio>
#include <cstdlib>
#include <iostream>

using std::cerr;
using std::endl;
using std::atoi;

namespace {

class Handler {
public:
	Handler(FlowInboundMap *inbound, FlowOutboundMap *outbound)
		: inbound_(inbound)
		, outbound_(outbound)
	{}

	bool Handle(int source, int target) {
		if ( (*inbound_)[target].insert(source).second &&
			 (*outbound_)[source].insert(target).second )
			return true;
		cerr << "duplicate entries in flows: "
			 << source
			 << " -> "
			 << target
			 << endl;
		return false;
	}

private:
	FlowInboundMap *inbound_;
	FlowOutboundMap *outbound_;
};

int Process(void *data, int argc, char **argv, char **names)
{
	Handler *h = static_cast<Handler *>(data);
	(void)names;
	assert(argc == 2);
	assert(argv[0]);
	assert(argv[1]);
	int source = atoi(argv[0]);
	int target = atoi(argv[1]);
	return h->Handle(source, target) ? 0 : 1;
}

}

bool LoadFlows(sqlite3 *db, FlowInboundMap *im, FlowOutboundMap *om)
{
	Handler h(im, om);
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT * FROM flows", &Process, &h, &em);
	if (e != SQLITE_OK) {
		cerr << "failed to select flows: " << e
			 << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
	return true;
}
