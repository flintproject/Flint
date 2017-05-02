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

class History;
class Locater;
class Mounter;

namespace cas {
class System;
}

namespace solver {
namespace ark {
class Mmdm;
}
}

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

	bool SpecifyCapacity(size_t size, History *history) const;

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
	bool GenerateMmdm(const cas::System &system, solver::ark::Mmdm *mmdm) const;

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
