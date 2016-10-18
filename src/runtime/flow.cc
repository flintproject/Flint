/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flow.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "reduction.h"

namespace flint {
namespace {

class Handler {
public:
	explicit Handler(FlowInboundMap *inbound)
		: inbound_(inbound)
	{}

	bool Handle(int source, int target, Reduction reduction, int size) {
		auto &c = (*inbound_)[target];
		c.reduction = reduction;
		if (!c.sources.insert(source).second) {
			std::cerr << "duplicate entries in flows: "
				 << source
				 << " -> "
				 << target
				 << std::endl;
			return false;
		}
		c.size = size;
		return true;
	}

private:
	FlowInboundMap *inbound_;
};

int Process(void *data, int argc, char **argv, char **names)
{
	Handler *h = static_cast<Handler *>(data);
	(void)names;
	assert(argc == 4);
	assert(argv[0]);
	assert(argv[1]);
	// argv[2] can be null
	assert(argv[3]);
	int source = std::atoi(argv[0]);
	int target = std::atoi(argv[1]);
	Reduction reduction = (argv[2]) ? static_cast<Reduction>(std::atoi(argv[2])) : Reduction::kUnspecified;
	int size = std::atoi(argv[3]);
	return h->Handle(source, target, reduction, size) ? 0 : 1;
}

}

bool LoadFlows(sqlite3 *db, FlowInboundMap *im)
{
	Handler h(im);
	char *em;
	int e;
	e = sqlite3_exec(db, "SELECT * FROM flows", &Process, &h, &em);
	if (e == SQLITE_OK)
		return true;
	if (e != SQLITE_ABORT)
		std::cerr << "failed to select flows: " << e
			 << ": " << em << std::endl;
	sqlite3_free(em);
	return false;
}

}
