/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "layout.h"

namespace flint {

bool Layout::ContainsDependentVariable() const
{
	for (const auto &dp : dv_) {
		switch (dp->type()) {
		case lo::S:
		case lo::T:
			break;
		default:
			return true;
		}
	}
	return false;
}

size_t Layout::MarkConstant(int nol, size_t size, char *arr) const
{
	size_t num_of_on = 0;
	size_t offset = kOffsetBase;
	int di = 0;
	for (const auto &tp : tv_) {
		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;

		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				assert(offset < size);
				switch (dp->type()) {
				case lo::S:
					for (int j=0;j<nol;j++) {
						arr[offset + (j * size)] = 1;
						num_of_on++;
					}
					break;
				default:
					// nothing to do
					break;
				}
				offset += dp->size();
			}
		}
	}
	assert(offset == size);
	return num_of_on;
}

long Layout::SelectStates(std::vector<std::pair<int, int> > *states) const
{
	long total = 0;
	size_t offset = kOffsetBase;
	int di = 0;
	for (const auto &tp : tv_) {
		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;
		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				int data_size = dp->size();
				switch (dp->type()) {
				case lo::X:
					if (states)
						states->push_back(std::make_pair(offset, data_size));
					total += data_size;
					break;
				default:
					break;
				}
				offset += data_size;
			}
		}
	}
	return total;
}

}
