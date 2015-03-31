/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LO_LAYOUT_LOADER_H_
#define FLINT_LO_LAYOUT_LOADER_H_

#include <memory>
#include <string>
#include <fstream>

#include <boost/noncopyable.hpp>

#include "bc/pack.h"

class LayoutLoader : boost::noncopyable {
public:
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
				std::auto_ptr<lo::Track> track(new lo::Track);
				if (!UnpackFromIstream(*track, &ifs_)) {
					cerr << "could not read Track" << endl;
					return false;
				}
				nos = track->nos();
				nod = track->nod();
				layout->AddTrack(track.release());
			}

			for (int di=0;di<nod;di++) {
				std::auto_ptr<lo::Data> data(new lo::Data);
				if (!UnpackFromIstream(*data, &ifs_)) {
					cerr << "could not read Data" << endl;
					return false;
				}
				layout->AddData(data.release());
			}
			for (int si=0;si<nos;si++) {
				std::auto_ptr<lo::Sector> sector(new lo::Sector);
				if (!UnpackFromIstream(*sector, &ifs_)) {
					cerr << "could not read Sector" << endl;
					return false;
				}
				layout->AddSector(sector.release());
			}
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

#endif
