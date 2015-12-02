/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_ARK_MASS_H_
#define FLINT_SOLVER_ARK_MASS_H_

#include <memory>
#include <utility>
#include <vector>

#include <arkode/arkode_direct.h>

namespace flint {

class Processor;

namespace solver {
namespace ark {

class MassExecutor;
class Mmdm;

class Mass {
public:
	Mass(const Mass &) = delete;
	Mass &operator=(const Mass &) = delete;

	Mass(Processor *processor, Mmdm *mmdm);

	~Mass();

	bool Evaluate(double *data);

	void Write(const double *data, DlsMat M);

private:
	Processor *processor_;
	Mmdm *mmdm_;
	std::unique_ptr<MassExecutor> executor_;
	std::unique_ptr<double[]> tmp_;
};

}
}
}

#endif
