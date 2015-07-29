/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_CALCULATION_UNIT_HH_
#define FLINT_RUNTIME_CALCULATION_UNIT_HH_

class CalculationUnit {
public:
	CalculationUnit(int section_index, int sector_index, int block_index)
		: section_index_(section_index)
		, sector_index_(sector_index)
		, block_index_(block_index)
	{}

	int section_index() const {return section_index_;}
	int sector_index() const {return sector_index_;}
	int block_index() const {return block_index_;}

private:
	int section_index_;
	int sector_index_;
	int block_index_;
};

#endif
