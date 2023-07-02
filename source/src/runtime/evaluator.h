/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_EVALUATOR_H_
#define FLINT_RUNTIME_EVALUATOR_H_

#include <memory>
#include <vector>

#include "flint/ct.h"
#include "lo/layout.h"
#include "runtime/flow.h"
#include "runtime/timeseries.h"
#include "sqlite3.h"

namespace flint {
namespace runtime {

class Evaluator {
public:
	const Layout &layout() const {return layout_;}

	bool Load(const boost::filesystem::path &layout_file);

	bool Evaluate(sqlite3 *db,
				  ct::Availability availability,
				  int seed,
				  Bytecode *bytecode,
				  const FlowInboundMap *inbound,
				  const TimeseriesVector *tv,
				  std::vector<double> *data);

private:
	Layout layout_;
	size_t layer_size_;
	DataOffsetMap dom_;
	SectorOffsetMap som_;
};

}
}

#endif
