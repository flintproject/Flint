/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_EVALUATOR_H_
#define FLINT_RUNTIME_EVALUATOR_H_

#include <memory>

#include "flint/ct.h"
#include "lo/layout.h"
#include "sqlite3.h"

namespace flint {
namespace runtime {

class Evaluator {
public:
	const Layout &layout() const {return layout_;}

	bool Load(const char *layout_file);

	bool Evaluate(sqlite3 *db,
				  ct::Availability availability,
				  int seed,
				  const char *bc_file,
				  const char *output_file,
				  const char *input_file = nullptr);

private:
	Layout layout_;
	size_t layer_size_;
	DataOffsetMap dom_;
	SectorOffsetMap som_;
};

}
}

#endif
