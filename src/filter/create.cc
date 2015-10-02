/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "filter.hh"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_set>

#include <boost/ptr_container/ptr_unordered_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"

#include "bc/index.h"
#include "db/read-only-driver.hh"
#include "db/statement-driver.hh"
#include "filter/spec_loader.h"
#include "lo/layout_loader.h"

using std::cerr;
using std::endl;
using std::memcpy;
using std::set;
using std::string;

namespace flint {
namespace filter {

namespace {

class TimeUnitLoader : db::StatementDriver {
public:
	// Note that db is for read only.
	TimeUnitLoader(sqlite3 *db)
		: db::StatementDriver(db, "SELECT * from time_unit")
	{
	}

	bool Load(string *time_unit) {
		int e = sqlite3_step(stmt());
		if (e == SQLITE_DONE) {
			// nothing to do
			sqlite3_reset(stmt());
			return true;
		}
		if (e == SQLITE_ROW) {
			const unsigned char *name = sqlite3_column_text(stmt(), 0);
			*time_unit = string((const char *)name);
			sqlite3_reset(stmt());
			return true;
		}
		cerr << "failed to step statement: " << e << endl;
		return false;
	}
};

class Spec {
public:
	Spec(const Spec &) = delete;
	Spec &operator=(const Spec &) = delete;

	Spec() {}

	void AddSpec(const boost::uuids::uuid &u, const char *name) {
		m_[u].insert(name);
	}

	bool Contains(const boost::uuids::uuid &id, const string &name) const {
		boost::ptr_unordered_map<boost::uuids::uuid, std::unordered_set<string> >::const_iterator it = m_.find(id);
		if (it == m_.end()) return false;
		return it->second->find(name) != it->second->end();
	}

private:
	boost::ptr_unordered_map<boost::uuids::uuid, std::unordered_set<string> > m_;
};

class Layout {
public:
	Layout(const Layout &) = delete;
	Layout &operator=(const Layout &) = delete;

	Layout() {}

	void AddTrack(lo::Track *track) {
		tv_.push_back(track);
	}

	void AddSector(lo::Sector *sector) {
		sv_.push_back(sector);
	}

	void AddData(lo::Data *data) {
		dv_.push_back(data);
	}

	int Fill(const string &time_unit, const Spec *spec, boost::ptr_vector<lo::Column> *columns) const {
		std::unique_ptr<lo::Column> c(new lo::Column);

		char us0[boost::uuids::uuid::static_size()] = {0};
		c->set_position(0);
		c->set_size(1);
		c->set_uuid(us0, boost::uuids::uuid::static_size());
		c->set_id(0); // no such value, let's use 0 as a fallback
		c->set_name("time");
		c->set_type(lo::T);
		c->set_unit(time_unit);
		columns->push_back(c.release());

		int si = 0;
		int di = 0;
		int pos = kOffsetBase;
		for (TrackVector::const_iterator it=tv_.begin();it!=tv_.end();++it) {
			int nos = it->nos();
			int nod = it->nod();
			int sie = si + nos;
			int dib = di;
			int die = di + nod;

			while (si < sie) {
				const lo::Sector &s = sv_.at(si++);
				boost::uuids::uuid su;
				memcpy(&su, s.id().data(), su.size());
				di = dib;
				while (di < die) {
					const lo::Data &d = dv_.at(di++);
					if (spec->Contains(su, d.name())) {
						c.reset(new lo::Column);
						c->set_position(pos);
						c->set_size(d.size());
						c->set_uuid(s.id().data(), boost::uuids::uuid::static_size());
						c->set_id(d.id());
						c->set_name(d.name());
						c->set_type(d.type());
						c->set_unit(d.unit());
						if (s.has_label()) c->set_label(s.label());
						columns->push_back(c.release());
					}
					pos += d.size();
				}
			}
		}
		return pos;
	}

private:
	typedef boost::ptr_vector<lo::Track> TrackVector;
	typedef boost::ptr_vector<lo::Sector> SectorVector;
	typedef boost::ptr_vector<lo::Data> DataVector;

	TrackVector tv_;
	SectorVector sv_;
	DataVector dv_;
};

}

bool Create(sqlite3 *db, const char *spec_file, const char *layout_file, const char *output_file)
{
	string time_unit;
	{
		TimeUnitLoader loader(db);
		if (!loader.Load(&time_unit)) return false;
	}

	std::unique_ptr<Spec> spec(new Spec);
	// load spec
	{
		std::unique_ptr<SpecLoader> loader(new SpecLoader(spec_file));
		if (!loader->Load(spec.get())) return false;
	}

	// load layout
	std::unique_ptr<Layout> layout(new Layout);
	{
		std::unique_ptr<LayoutLoader> loader(new LayoutLoader(layout_file));
		if (!loader->Load(layout.get())) return false;
	}

	std::unique_ptr<boost::ptr_vector<lo::Column> > columns(new boost::ptr_vector<lo::Column>);
	int size = layout->Fill(time_unit, spec.get(), columns.get());
	std::unique_ptr<lo::Header> header(new lo::Header);
	header->set_size(size);

	std::ofstream ofs(output_file, std::ios::out|std::ios::binary);
	if (!ofs.is_open()) {
		cerr << "could not open output file: " << output_file << endl;
		return false;
	}
	if (!PackToOstream(*header, &ofs)) {
		cerr << "failed to pack Header" << endl;
		return false;
	}
	for (boost::ptr_vector<lo::Column>::const_iterator it=columns->begin();it!=columns->end();++it) {
		if (!PackToOstream(*it, &ofs)) {
			cerr << "failed to pack Column" << endl;
			return false;
		}
	}
	ofs.close();
	return true;
}

}
}
