/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "layout.h"

size_t Layout::MarkConstant(int nol, size_t size, char *arr) const
{
	size_t num_of_on = 0;
	size_t offset = kOffsetBase;
	int di = 0;
	for (TrackVector::const_iterator it=tv_.begin();it!=tv_.end();++it) {
		int nos = it->nos();
		int nod = it->nod();
		int dib = di;
		int die = di + nod;

		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const lo::Data &d = dv_.at(di++);
				assert(offset < size);
				switch (d.type()) {
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
				offset += d.size();
			}
		}
	}
	assert(offset == size);
	return num_of_on;
}

void Layout::CollectVariable(size_t size, std::vector<int> *offsets) const
{
	size_t offset = kOffsetBase;
	int di = 0;
	for (TrackVector::const_iterator it=tv_.begin();it!=tv_.end();++it) {
		int nos = it->nos();
		int nod = it->nod();
		int dib = di;
		int die = di + nod;

		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const lo::Data &d = dv_.at(di++);
				assert(offset < size);
				switch (d.type()) {
				case lo::S:
					// nothing to do
					break;
				default:
					offsets->push_back(offset);
					break;
				}
				offset += d.size();
			}
		}
	}
	assert(offset == size);
}
