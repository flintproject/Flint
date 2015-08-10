/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LO_LAYOUT_H_
#define FLINT_LO_LAYOUT_H_

#include <algorithm>
#include <cstring>
#include <iomanip>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_unordered_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "bc/index.h"
#include "bc/locater.h"
#include "bc/mounter.h"
#include "lo.pb.h"

namespace flint {

typedef boost::ptr_unordered_map<boost::uuids::uuid,
								 std::unordered_map<int, int>
								 > DataOffsetMap;
typedef std::unordered_map<boost::uuids::uuid,
						   int,
						   boost::hash<boost::uuids::uuid>
						   > SectorOffsetMap;

class Layout : boost::noncopyable {
public:
	void AddTrack(lo::Track *track) {
		tv_.push_back(track);
	}

	void AddSector(lo::Sector *sector) {
		sv_.push_back(sector);
	}

	void AddData(lo::Data *data) {
		dv_.push_back(data);
	}

	int Calculate(DataOffsetMap *dom = NULL, SectorOffsetMap *som = NULL) {
		int offset = kOffsetBase;
		int di = 0;
		int si = 0;
		boost::uuids::uuid track_id;
		boost::uuids::uuid sector_id;
		for (TrackVector::const_iterator it=tv_.begin();it!=tv_.end();++it) {
			memcpy(&track_id, it->id().data(), track_id.size());

			int nos = it->nos();
			int nod = it->nod();
			int die = di + nod;
			int sie = si + nos;
			int sector_size = 0;

			std::unique_ptr<Locater> locater(new Locater);
			while (di < die) {
				const lo::Data &d = dv_.at(di++);
				locater->SetPosition(d.name(), sector_size);
				if (dom) (*dom)[track_id].insert(std::make_pair(d.id(), sector_size));
				sector_size += d.size();
			}
			lm_.insert(track_id, locater.release());

			std::unique_ptr<Mounter> mounter(new Mounter(nos));
			for (int i=0;i<nos;i++) {
				mounter->SetOffset(i, offset);
				const lo::Sector &s = sv_.at(si++);
				if (som) {
					memcpy(&sector_id, s.id().data(), sector_id.size());
					som->insert(std::make_pair(sector_id, offset));
				}
				offset += sector_size;
			}
			mm_.insert(track_id, mounter.release());
			assert(si == sie);
		}
		return offset;
	}

	const Locater &GetLocater(const std::string &id) const {
		boost::uuids::uuid u;
		std::memcpy(&u, id.data(), u.size());
		return lm_.at(u);
	}

	const Mounter &GetMounter(const std::string &id) const {
		boost::uuids::uuid u;
		std::memcpy(&u, id.data(), u.size());
		return mm_.at(u);
	}

	template<typename THistory>
	bool SpecifyCapacity(size_t size, THistory *history) const {
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
					if (offset >= size) {
						std::cerr << "exceed history size: " << size << std::endl;
						return false;
					}
					if (d.has_capacity()) {
						history[offset].set_capacity(d.capacity());
					}
					offset += d.size();
				}
			}
		}
		if (offset != size) {
			std::cerr << "failed to specify capacity at the end" << std::endl;
			return false;
		}
		return true;
	}

	void DetectRed(size_t size, const size_t *color) const {
		std::unordered_set<std::string> red;

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
					if (!color[offset]) {
						boost::uuids::uuid u;
						std::memcpy(&u, it->id().data(), u.size());
						std::string us = to_string(u);
						us += ":";
						us += d.name();
						if (red.insert(us).second) {
							std::cerr << us << std::endl;
						}
					}
					offset += d.size();
				}
			}
		}
		assert(offset == size);
	}

	void CollectConstant(int nol, size_t size, std::set<int> *addrs) const {
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
							addrs->insert(offset + (j * size));
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
	}

	/*
	 * Return the number of "on".
	 */
	size_t MarkConstant(int nol, size_t size, char *arr) const;

	void CollectVariable(size_t size, std::vector<int> *offsets) const;

	void Debug(size_t size) const {
		using std::cout;
		using std::endl;

		size_t offset = kOffsetBase;
		int si = 0;
		int di = 0;
		for (TrackVector::const_iterator it=tv_.begin();it!=tv_.end();++it) {
			boost::uuids::uuid u;
			memcpy(&u, it->id().data(), u.size());
			cout << "T " << u << " " << it->name() << endl;

			int nos = it->nos();
			int nod = it->nod();
			int dib = di;
			int die = di + nod;

			for (int i=0;i<nos;i++) {
				memcpy(&u, sv_[si++].id().data(), u.size());
				cout << "S " << u << endl;

				di = dib;
				while (di < die) {
					const lo::Data &d = dv_.at(di++);
					assert(offset < size);
					cout << "|" << std::setw(4) << offset;
					switch (d.type()) {
					case lo::S:
						cout << "|S|";
						break;
					case lo::T:
						cout << "|T|";
						break;
					case lo::V:
						cout << "|V|";
						break;
					case lo::X:
						cout << "|X|";
						break;
					}
					cout << d.name() << endl;
					offset += d.size();
				}
			}
		}
		assert(offset == size);
	}

private:
	typedef boost::ptr_vector<lo::Track> TrackVector;
	typedef boost::ptr_vector<lo::Sector> SectorVector;
	typedef boost::ptr_vector<lo::Data> DataVector;
	typedef boost::ptr_unordered_map<boost::uuids::uuid, Locater> LocaterMap;
	typedef boost::ptr_unordered_map<boost::uuids::uuid, Mounter> MounterMap;

	TrackVector tv_;
	SectorVector sv_;
	DataVector dv_;
	LocaterMap lm_;
	MounterMap mm_;
};

}

#endif
