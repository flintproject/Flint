/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "layout.h"

namespace flint {

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

}
