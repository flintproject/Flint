/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flow.hh"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "reduction.hh"

using std::cerr;
using std::endl;
using std::atoi;

namespace flint {
namespace {

class Handler {
public:
	Handler(FlowInboundMap *inbound)
		: inbound_(inbound)
	{}

	bool Handle(int source, int target, Reduction reduction) {
		auto &p = (*inbound_)[target];
		p.first = reduction;
		if (p.second.insert(source).second)
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
};

int Process(void *data, int argc, char **argv, char **names)
{
	Handler *h = static_cast<Handler *>(data);
	(void)names;
	assert(argc == 3);
	assert(argv[0]);
	assert(argv[1]);
	// argv[2] can be null
	int source = atoi(argv[0]);
	int target = atoi(argv[1]);
	Reduction reduction = (argv[2]) ? static_cast<Reduction>(atoi(argv[2])) : Reduction::kUnspecified;
	return h->Handle(source, target, reduction) ? 0 : 1;
}

}

bool LoadFlows(sqlite3 *db, FlowInboundMap *im)
{
	Handler h(im);
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

}
