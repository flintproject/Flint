/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_ARK_RHS_H_
#define FLINT_SOLVER_ARK_RHS_H_

#include <memory>
#include <utility>
#include <vector>

namespace flint {

class Processor;

namespace solver {
namespace ark {

class RhsExecutor;

class Rhs {
public:
	Rhs(const Rhs &) = delete;
	Rhs &operator=(const Rhs &) = delete;

	Rhs(int layer_size, Processor *processor);

	~Rhs();

	bool Evaluate(double *data);

private:
	Processor *processor_;
	std::unique_ptr<RhsExecutor> executor_;
	std::unique_ptr<intptr_t[]> ir_;
	std::unique_ptr<double[]> tmp_;
};

}
}
}

#endif
