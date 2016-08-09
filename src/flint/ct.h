/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CT_H_
#define FLINT_CT_H_

#include "flint/bc.h"
#include "runtime/execution-unit.h"
#include "runtime/flow.h"

#include <memory>
#include <vector>

namespace flint {

class CalculationDependency;
class Layout;

namespace ct {

typedef std::vector<std::unique_ptr<ReductionUnit> > ReductionUnitVector;

typedef std::vector<std::unique_ptr<CalculationDependency> > CalculationDependencyVector;

enum class Availability {
	kNone = 0,
	kLiteral = 1,
	kConstant = 2
};

class DataFlowAnalyzer {
public:
	DataFlowAnalyzer(const DataFlowAnalyzer &) = delete;
	DataFlowAnalyzer &operator=(const DataFlowAnalyzer &) = delete;

	DataFlowAnalyzer(const Layout *layout, int layer_size, Bytecode *bytecode);

	/*
	 * Returns true if there is no code at all, false otherwise.
	 */
	bool IsEmpty() const;

	void CalculateCodeOffset();

	/*
	 * This function drops some elements of shv_, bhv_, and cv_ not associated
	 * with any existing sector, thus should be called first.
	 */
	bool SolveLocation();

	int GetMaxNoir();

	int GetMaxNumberOfData();

	bool SolveDependencies(const FlowInboundMap *inbound,
						   Availability availability = Availability::kConstant,
						   size_t *color = nullptr);

	void ScheduleEvents(const FlowInboundMap &inbound);

private:
	void CollectCalculationDependencies(CalculationDependencyVector *cdv);

	void CollectReductionUnits(const FlowInboundMap *inbound,
							   ReductionUnitVector *ruv);

protected:
	int GetNol() const;
	ShVector &GetShv();
	BhVector &GetBhv();
	CVector &GetCv();

	const Layout *layout_;
	size_t layer_size_;
	Bytecode *bytecode_;

	std::vector<ExecutionUnit> euv_;

	std::unique_ptr<int[]> code_offset_;
};

}
}

#endif
