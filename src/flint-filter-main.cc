/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <set>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"

#include "bc/index.h"
#include "db/driver.h"
#include "filter/spec_loader.h"
#include "lo/layout_loader.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::memcpy;
using std::set;
using std::string;

namespace {

class TimeUnitLoader : boost::noncopyable {
public:
	TimeUnitLoader(sqlite3 *db)
		: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db, "SELECT * from time_unit",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			exit(EXIT_FAILURE);
		}
	}

	~TimeUnitLoader() {
		sqlite3_finalize(stmt_);
	}

	bool Load(string *time_unit) {
		int e = sqlite3_step(stmt_);
		if (e == SQLITE_DONE) {
			// nothing to do
			sqlite3_reset(stmt_);
			return true;
		}
		if (e == SQLITE_ROW) {
			const unsigned char *name = sqlite3_column_text(stmt_, 0);
			*time_unit = string((const char *)name);
			sqlite3_reset(stmt_);
			return true;
		}
		cerr << "failed to step statement: " << e << endl;
		return false;
	}

private:
	sqlite3_stmt *stmt_;
};

class Spec : boost::noncopyable {
public:
	void AddSpec(const boost::uuids::uuid &u, const char *name) {
		char s[16];
		std::copy(u.begin(), u.end(), s);
		string id(s, 16);
		m_[id].insert(name);
	}

	bool Contains(const string &id, const string &name) const {
		boost::ptr_map<string, set<string> >::const_iterator it = m_.find(id);
		if (it == m_.end()) return false;
		return it->second->count(name) > 0;
	}

private:
	boost::ptr_map<string, set<string> > m_;
};

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

	int Fill(const string &time_unit, const Spec *spec, boost::ptr_vector<lo::Column> *columns) const {
		std::auto_ptr<lo::Column> c(new lo::Column);

		char us0[16] = {0};
		c->set_position(0);
		c->set_size(1);
		c->set_uuid(string(us0, 16));
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
				di = dib;
				while (di < die) {
					const lo::Data &d = dv_.at(di++);
					if (spec->Contains(s.id(), d.name())) {
						c.reset(new lo::Column);
						c->set_position(pos);
						c->set_size(d.size());
						c->set_uuid(s.id());
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

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string db_file, spec_file, layout_file, output_file;
	int print_help = 0;

	opts.add_options()
		("db", po::value<string>(&db_file), "Input database file")
		("spec", po::value<string>(&spec_file), "Input spec file")
		("layout", po::value<string>(&layout_file), "Input layout file")
		("output", po::value<string>(&output_file), "Output file")
		("help,h", "Show this message");
	popts.add("db", 1).add("spec", 1).add("layout", 1).add("output", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("db") == 0 ||
					vm.count("spec") == 0 ||
					vm.count("layout") == 0 ||
					vm.count("output") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " DB SPEC LAYOUT OUTPUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	string time_unit;
	{
		boost::scoped_ptr<db::Driver> driver(new db::Driver(db_file.c_str()));
		{
			boost::scoped_ptr<TimeUnitLoader> loader(new TimeUnitLoader(driver->db()));
			if (!loader->Load(&time_unit)) return EXIT_FAILURE;
		}
	}

	boost::scoped_ptr<Spec> spec(new Spec);
	// load spec
	{
		boost::scoped_ptr<SpecLoader> loader(new SpecLoader(spec_file));
		if (!loader->Load(spec.get())) return EXIT_FAILURE;
	}

	// load layout
	boost::scoped_ptr<Layout> layout(new Layout);
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(layout_file));
		if (!loader->Load(layout.get())) return EXIT_FAILURE;
	}

	boost::scoped_ptr<boost::ptr_vector<lo::Column> > columns(new boost::ptr_vector<lo::Column>);
	int size = layout->Fill(time_unit, spec.get(), columns.get());
	boost::scoped_ptr<lo::Header> header(new lo::Header);
	header->set_size(size);

	std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
	if (!ofs.is_open()) {
		cerr << "could not open output file: " << output_file << endl;
		return EXIT_FAILURE;
	}
	if (!PackToOstream(*header, &ofs)) {
		cerr << "failed to pack Header" << endl;
		return EXIT_FAILURE;
	}
	for (boost::ptr_vector<lo::Column>::const_iterator it=columns->begin();it!=columns->end();++it) {
		if (!PackToOstream(*it, &ofs)) {
			cerr << "failed to pack Column" << endl;
			return EXIT_FAILURE;
		}
	}
	ofs.close();

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
