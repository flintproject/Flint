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

#include <boost/functional/hash.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "bc/index.h"
#include "bc/locater.h"
#include "bc/mounter.h"
#include "lo.pb.h"

namespace flint {

typedef std::unordered_map<boost::uuids::uuid,
						   std::unordered_map<int, int>,
						   boost::hash<boost::uuids::uuid>
						   > DataOffsetMap;
typedef std::unordered_map<boost::uuids::uuid,
						   int,
						   boost::hash<boost::uuids::uuid>
						   > SectorOffsetMap;

class Layout {
public:
	Layout(const Layout &) = delete;
	Layout &operator=(const Layout &) = delete;

	Layout() {}

	void AddTrack(std::unique_ptr<lo::Track> &&track) {
		tv_.push_back(std::move(track));
	}

	void AddSector(std::unique_ptr<lo::Sector> &&sector) {
		sv_.push_back(std::move(sector));
	}

	void AddData(std::unique_ptr<lo::Data> &&data) {
		dv_.push_back(std::move(data));
	}

	/*
	 * Return true if the layout has any dependent variable, false otherwise.
	 */
	bool ContainsDependentVariable() const;

	int Calculate(DataOffsetMap *dom = NULL, SectorOffsetMap *som = NULL) {
		int offset = kOffsetBase;
		int di = 0;
		int si = 0;
		boost::uuids::uuid track_id;
		boost::uuids::uuid sector_id;
		for (const auto &tp : tv_) {
			memcpy(&track_id, tp->id().data(), track_id.size());

			int nos = tp->nos();
			int nod = tp->nod();
			int die = di + nod;
			int sie = si + nos;
			int sector_size = 0;

			std::unique_ptr<Locater> locater(new Locater);
			while (di < die) {
				const auto &dp = dv_.at(di++);
				locater->SetPosition(dp->name(), sector_size);
				if (dom) (*dom)[track_id].insert(std::make_pair(dp->id(), sector_size));
				sector_size += dp->size();
			}
			lm_.insert(std::make_pair(track_id, std::move(locater)));

			std::unique_ptr<Mounter> mounter(new Mounter(nos));
			for (int i=0;i<nos;i++) {
				mounter->SetOffset(i, offset);
				const auto &sp = sv_.at(si++);
				if (som) {
					memcpy(&sector_id, sp->id().data(), sector_id.size());
					som->insert(std::make_pair(sector_id, offset));
				}
				offset += sector_size;
			}
			mm_.insert(std::make_pair(track_id, std::move(mounter)));
			assert(si == sie);
		}
		return offset;
	}

	const Locater *GetLocater(const std::string &id) const {
		boost::uuids::uuid u;
		std::memcpy(&u, id.data(), u.size());
		LocaterMap::const_iterator it = lm_.find(u);
		if (it == lm_.end()) return nullptr;
		return it->second.get();
	}

	const Mounter &GetMounter(const std::string &id) const {
		boost::uuids::uuid u;
		std::memcpy(&u, id.data(), u.size());
		assert(mm_.find(u) != mm_.end());
		return *mm_.at(u);
	}

	template<typename THistory>
	bool SpecifyCapacity(size_t size, THistory *history) const {
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
					if (offset >= size) {
						std::cerr << "exceed history size: " << size << std::endl;
						return false;
					}
					if (dp->has_capacity()) {
						history[offset].set_capacity(dp->capacity());
					}
					offset += dp->size();
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
					if (!color[offset]) {
						boost::uuids::uuid u;
						std::memcpy(&u, tp->id().data(), u.size());
						std::string us = to_string(u);
						us += ":";
						us += dp->name();
						if (red.insert(us).second) {
							std::cerr << us << std::endl;
						}
					}
					offset += dp->size();
				}
			}
		}
		assert(offset == size);
	}

	void CollectConstant(int nol, size_t size, std::set<int> *addrs) const {
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
							addrs->insert(offset + (j * size));
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
	}

	/*
	 * Return the number of "on".
	 */
	size_t MarkConstant(int nol, size_t size, char *arr) const;

	/*
	 * Select the locations occupied as a variable of type 'X'.
	 * A location consists of its offset and size.
	 * Return its total size i.e. the number of scalar states.
	 */
	long SelectStates(std::vector<std::pair<int, int> > *states = nullptr) const;

	/*
	 * Generate mass-matrix data map.
	 */
	template<typename TSystem, typename TMmdm>
	bool GenerateMmdm(const TSystem &system, TMmdm *mmdm) const {
		long index = 0; // in global mass matrix
		size_t offset = kOffsetBase;
		int di = 0;
		boost::uuids::uuid track_id;
		for (const auto &tp : tv_) {
			memcpy(&track_id, tp->id().data(), track_id.size());

			int nos = tp->nos();
			int nod = tp->nod();
			int dib = di;
			int die = di + nod;

			int pos = 0;
			std::unordered_map<std::string, int> am;
			std::unordered_map<std::string, std::string> xm;
			for (int i=0;i<nod;i++) {
				const auto &dp = dv_.at(di++);
				int data_size = dp->size();
				switch (dp->type()) {
				case lo::X:
					{
						std::string m;
						if (!system.FindMass(track_id, dp->name(), &m))
							return false;
						xm.insert(std::make_pair(dp->name(), m));
					}
					break;
				default:
					break;
				}
				am.insert(std::make_pair(dp->name(), pos));
				pos += data_size;
			}
			assert(di == die);

			int bot = offset; // begin of track
			for (int i=0;i<nos;i++) {
				di = dib;
				while (di < die) {
					const auto &dp = dv_.at(di++);
					int data_size = dp->size();
					switch (dp->type()) {
					case lo::X:
						{
							const std::string &name = xm.at(dp->name());
							if (name.empty()) { // identity
								// column-major
								for (int col=0;col<data_size;col++) {
									for (int row=0;row<data_size;row++) {
										if (row == col)
											mmdm->Add(index + row, index + col, 1);
									}
								}
							} else {
								int m = am.at(name);
								// column-major
								for (int col=0;col<data_size;col++) {
									for (int row=0;row<data_size;row++) {
										mmdm->Add(index + row, index + col,
												  bot + (pos * i) + m + (col * data_size) + row);
									}
								}
							}
						}
						index += data_size;
						break;
					default:
						break;
					}
					offset += data_size;
				}
			}
		}
		return true;
	}

	void Debug(size_t size) const {
		using std::cout;
		using std::endl;

		size_t offset = kOffsetBase;
		int si = 0;
		int di = 0;
		for (const auto &tp : tv_) {
			boost::uuids::uuid u;
			memcpy(&u, tp->id().data(), u.size());
			cout << "T " << u << " " << tp->name() << endl;

			int nos = tp->nos();
			int nod = tp->nod();
			int dib = di;
			int die = di + nod;

			for (int i=0;i<nos;i++) {
				memcpy(&u, sv_[si++]->id().data(), u.size());
				cout << "S " << u << endl;

				di = dib;
				while (di < die) {
					const auto &dp = dv_.at(di++);
					assert(offset < size);
					cout << "|" << std::setw(4) << offset;
					switch (dp->type()) {
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
					cout << dp->name() << endl;
					offset += dp->size();
				}
			}
		}
		assert(offset == size);
	}

private:
	typedef std::vector<std::unique_ptr<lo::Track> > TrackVector;
	typedef std::vector<std::unique_ptr<lo::Sector> > SectorVector;
	typedef std::vector<std::unique_ptr<lo::Data> > DataVector;
	typedef std::unordered_map<boost::uuids::uuid,
							   std::unique_ptr<Locater>,
							   boost::hash<boost::uuids::uuid>
							   > LocaterMap;
	typedef std::unordered_map<boost::uuids::uuid,
							   std::unique_ptr<Mounter>,
							   boost::hash<boost::uuids::uuid>
							   > MounterMap;

	TrackVector tv_;
	SectorVector sv_;
	DataVector dv_;
	LocaterMap lm_;
	MounterMap mm_;
};

}

#endif
