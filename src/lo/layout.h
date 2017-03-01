/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LO_LAYOUT_H_
#define FLINT_LO_LAYOUT_H_

#include <cassert>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/functional/hash.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "bc/index.h"
#include "fppp.h"
#include "lo.pb.h"

namespace flint {

class Locater;
class Mounter;

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

	Layout();

	~Layout();

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

	int Calculate(DataOffsetMap *dom = nullptr, SectorOffsetMap *som = nullptr);

	const Locater *GetLocater(const std::string &id) const;

	const Mounter &GetMounter(const std::string &id) const;

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
					offset += dp->col() * dp->row();
				}
			}
		}
		if (offset != size) {
			std::cerr << "failed to specify capacity at the end" << std::endl;
			return false;
		}
		return true;
	}

	void DetectRed(size_t size, const size_t *color) const;

	void CollectConstant(int nol, size_t size, std::set<int> *addrs) const;

	/*
	 * Return the number of "on".
	 */
	size_t MarkConstant(int nol, size_t size, int *levels) const;

	/*
	 * Return the number of "on".
	 */
	size_t MarkLiteral(int nol, size_t size, int *levels, size_t *color) const;

	/*
	 * Select the locations occupied as a variable of type 'X'.
	 * A location consists of its offset and size.
	 * Return its total size i.e. the number of scalar states.
	 */
	long SelectStates(std::vector<std::pair<int, int> > *states = nullptr) const;

	bool SelectByKeyData(std::map<fppp::KeyData, size_t> *output) const;

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
				switch (dp->type()) {
				case lo::X:
					{
						std::string m;
						if (!system.FindMass(track_id, dp->name(), &m))
							return false;
						xm.emplace(dp->name(), m);
					}
					break;
				default:
					break;
				}
				am.emplace(dp->name(), pos);
				pos += dp->col() * dp->row();
			}
			assert(di == die);

			int bot = offset; // begin of track
			for (int i=0;i<nos;i++) {
				di = dib;
				while (di < die) {
					const auto &dp = dv_.at(di++);
					int data_size = dp->col() * dp->row();
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

	void Debug(size_t size) const;

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
