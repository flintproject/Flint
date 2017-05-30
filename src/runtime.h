/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_H_
#define FLINT_RUNTIME_H_

#include "flint/ct.h"
#include "runtime/flow.h"
#include "runtime/timeseries.h"
#include "sqlite3.h"

#include <vector>

namespace flint {
namespace runtime {

/*
 * Return true in case of success, otherwise false.
 */
bool Eval(sqlite3 *db,
		  ct::Availability availability,
		  int seed,
		  const char *layout_file,
		  Bytecode *bytecode,
		  const FlowInboundMap *inbound,
		  const TimeseriesVector *tv,
		  std::vector<double> *data);

/*
 * Return true in case of success, otherwise false.
 */
bool Init(sqlite3 *db,
		  int seed,
		  const char *layout_file,
		  Bytecode *bytecode,
		  const FlowInboundMap *inbound,
		  const TimeseriesVector *tv,
		  std::vector<double> *data);

}
}

#endif
