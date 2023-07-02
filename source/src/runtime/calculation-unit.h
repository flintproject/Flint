/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_CALCULATION_UNIT_H_
#define FLINT_RUNTIME_CALCULATION_UNIT_H_

namespace flint {

class CalculationUnit {
public:
	CalculationUnit(int section_index, int block_index,
					int offset, int cib, int cie);

	int section_index() const {return section_index_;}
	int block_index() const {return block_index_;}
	int offset() const {return offset_;}
	int cib() const {return cib_;}
	int cie() const {return cie_;}

private:
	int section_index_;
	int block_index_;
	int offset_;
	int cib_;
	int cie_;
};

}

#endif
