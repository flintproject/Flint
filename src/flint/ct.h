/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CT_H_
#define FLINT_CT_H_

#include <memory>
#include <vector>

#include "bc/bc_loader.h"
#include "runtime/execution-unit.h"
#include "runtime/flow.h"

namespace flint {

class CalculationDependency;
class Layout;

namespace ct {

typedef std::vector<std::unique_ptr<ReductionUnit> > ReductionUnitVector;

typedef std::vector<std::unique_ptr<CalculationDependency> > CalculationDependencyVector;

class DataFlowAnalyzer {
public:
	DataFlowAnalyzer(const DataFlowAnalyzer &) = delete;
	DataFlowAnalyzer &operator=(const DataFlowAnalyzer &) = delete;

	DataFlowAnalyzer(const Layout *layout, int layer_size);

	ShVector *GetShv() const;
	BhVector *GetBhv() const;
	CVector *GetCv() const;

	void CalculateCodeOffset();

	/*
	 * This function drops some elements of shv_, bhv_, and cv_ not associated
	 * with any existing sector, thus should be called first.
	 */
	bool SolveLocation();

	int GetMaxNoir();

	int GetMaxNumberOfData();

	bool SolveDependencies(int nol,
						   const FlowInboundMap *inbound,
						   bool constantAvailable);

private:
	void CollectCalculationDependencies(CalculationDependencyVector *cdv);

	void CollectReductionUnits(int nol, const FlowInboundMap *inbound,
							   ReductionUnitVector *ruv);

protected:
	const Layout *layout_;
	size_t layer_size_;

	std::unique_ptr<ShVector> shv_;
	std::unique_ptr<BhVector> bhv_;
	std::unique_ptr<CVector> cv_;

	std::vector<ExecutionUnit> euv_;

	std::unique_ptr<int[]> code_offset_;
};

}
}

#endif
