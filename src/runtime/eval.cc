/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "runtime.h"

#include <memory>

#include "runtime/evaluator.h"

namespace flint {
namespace runtime {

bool Eval(sqlite3 *db,
		  ct::Availability availability,
		  int seed,
		  const char *layout_file,
		  Bytecode *bytecode,
		  const FlowInboundMap *inbound,
		  const TimeseriesVector *tv,
		  std::vector<double> *data)
{
	std::unique_ptr<Evaluator> e(new Evaluator);
	return e->Load(layout_file) &&
		e->Evaluate(db, availability, seed, bytecode, inbound, tv, data);
}

}
}
