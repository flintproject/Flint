/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_ARK_AUXV_H_
#define FLINT_SOLVER_ARK_AUXV_H_

#include <memory>
#include <utility>
#include <vector>

namespace flint {

class Processor;

namespace solver {
namespace ark {

class AuxvExecutor;

class Auxv {
public:
	Auxv(const Auxv &) = delete;
	Auxv &operator=(const Auxv &) = delete;

	explicit Auxv(Processor *processor);

	~Auxv();

	bool Evaluate(double *data);

private:
	Processor *processor_;
	std::unique_ptr<AuxvExecutor> executor_;
	std::unique_ptr<double[]> tmp_;
};

}
}
}

#endif
