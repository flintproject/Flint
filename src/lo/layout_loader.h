/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LO_LAYOUT_LOADER_H_
#define FLINT_LO_LAYOUT_LOADER_H_

#include <memory>
#include <string>
#include <fstream>

#include "bc/pack.h"

namespace flint {

class LayoutLoader {
public:
	LayoutLoader(const LayoutLoader &) = delete;
	LayoutLoader &operator=(const LayoutLoader &) = delete;

	explicit LayoutLoader(const std::string &file) : ifs_(file.c_str(), std::ios::in|std::ios::binary) {}

	~LayoutLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename TLayout>
	bool Load(TLayout *layout) {
		using std::cerr;
		using std::endl;

		if (!ifs_.is_open()) {
			cerr << "failed to open layout file" << endl;
			return false;
		}

		while (ifs_.peek() != EOF) {
			int nos = 0;
			int nod = 0;
			{
				std::unique_ptr<lo::Track> track(new lo::Track);
				if (!UnpackFromIstream(*track, &ifs_)) {
					cerr << "could not read Track" << endl;
					return false;
				}
				nos = track->nos();
				nod = track->nod();
				layout->AddTrack(std::move(track));
			}

			for (int di=0;di<nod;di++) {
				std::unique_ptr<lo::Data> data(new lo::Data);
				if (!UnpackFromIstream(*data, &ifs_)) {
					cerr << "could not read Data" << endl;
					return false;
				}
				layout->AddData(std::move(data));
			}
			for (int si=0;si<nos;si++) {
				std::unique_ptr<lo::Sector> sector(new lo::Sector);
				if (!UnpackFromIstream(*sector, &ifs_)) {
					cerr << "could not read Sector" << endl;
					return false;
				}
				layout->AddSector(std::move(sector));
			}
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

}

#endif
