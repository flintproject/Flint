/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.hh"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <string>

#include <boost/ptr_container/ptr_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"

#include "bc/index.h"
#include "db/statement-driver.h"
#include "lo/layout_loader.h"

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fread;
using std::fseek;
using std::make_pair;
using std::perror;
using std::printf;
using std::sscanf;
using std::string;
using std::strlen;

namespace job {

namespace {

class FormatLoader : db::StatementDriver {
public:
	// Note that db is for read only.
	explicit FormatLoader(sqlite3 *db)
		: db::StatementDriver(db, "SELECT format FROM model")
	{}

	// the return value should be freed by caller.
	char *Load()
	{
		int e;
		e = sqlite3_step(stmt());
		if (e != SQLITE_ROW) {
			cerr << "failed to step statement: " << e << endl;
			return NULL;
		}
		const char *f = (const char *)sqlite3_column_text(stmt(), 0);
		size_t len = strlen(f);
		char *format = new char[len+1];
		strcpy(format, f);
		return format;
	}
};

typedef std::map<int, int> TargetMap;

class SourceLayout : boost::noncopyable {
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

	int CollectTargets(TargetMap *tm) const {
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
					switch (d.type()) {
					case lo::S:
						if (std::strncmp(d.name().c_str(), "phsp:target", 11) == 0) {
							const char *nstr = d.name().c_str();
							int target_id = std::atoi(&nstr[11]);
							tm->insert(make_pair(target_id, pos));
						}
						break;
					default:
						cerr << "unexpected data type: " << d.type() << endl;
						return -1;
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

typedef boost::ptr_map<string, std::map<string, double> > TargetValueMap;

class TargetLoader : db::StatementDriver {
public:
	// Note that db is for read only.
	explicit TargetLoader(sqlite3 *db)
		: db::StatementDriver(db, "SELECT uuid, id FROM phsp_targets WHERE rowid = ?")
	{}

	bool Load(const TargetMap &tm, const double *data, TargetValueMap *tvm) {
		boost::uuids::uuid u;
		boost::uuids::string_generator gen;
		for (TargetMap::const_iterator it=tm.begin();it!=tm.end();++it) {
			int e = sqlite3_bind_int(stmt(), 1, it->first);
			if (e != SQLITE_OK) {
				cerr << "failed to bind rowid: " << e << endl;
				return false;
			}
			e = sqlite3_step(stmt());
			if (e != SQLITE_ROW) {
				cerr << "missing row with rowid " << it->first << " in phsp_targets" << endl;
				return false;
			}
			u = gen((const char *)sqlite3_column_text(stmt(), 0));
			(*tvm)[string((const char *)u.data, 16)].insert(make_pair((const char *)sqlite3_column_text(stmt(), 1), data[it->second]));
			sqlite3_reset(stmt());
		}
		return true;
	}
};

class TargetLayout : boost::noncopyable {
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

	bool Rewrite(const char *format, const TargetValueMap &tvm, FILE *fp) const {
		boost::scoped_array<char> buf(new char[32]); // FIXME
		int si = 0;
		int di = 0;
		if (fseek(fp, kOffsetBase*sizeof(double), SEEK_SET) != 0) {
			return false;
		}
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
					switch (d.type()) {
					case lo::S:
					case lo::X:
						{
							TargetValueMap::const_iterator it = tvm.find(s.id());
							if (it == tvm.end()) {
								if (fseek(fp, d.size()*sizeof(double), SEEK_CUR) != 0) {
									return false;
								}
							} else {
								std::map<string, double>::const_iterator mit;
								if (strcmp("phml", format) == 0) {
									sprintf(buf.get(), "%d", d.id());
									mit = it->second->find(buf.get());
								} else {
									mit = it->second->find(d.name());
								}
								if (mit == it->second->end()) {
									if (fseek(fp, d.size()*sizeof(double), SEEK_CUR) != 0) {
										return false;
									}
								} else {
									double value = mit->second;
									if (std::fwrite(&value, sizeof(double), d.size(), fp) != static_cast<size_t>(d.size())) {
										// TODO
										return false;
									}
								}
							}
						}
						break;
					default:
						if (fseek(fp, d.size()*sizeof(double), SEEK_CUR) != 0) {
							return false;
						}
						break;
					}
				}
			}
		}
		return true;
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

bool Store(sqlite3 *db,
		   const char *source_layout_file, const char *source_data_file,
		   const char *target_layout_file, const char *target_data_file)
{
	SourceLayout source_layout;
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(source_layout_file));
		if (!loader->Load(&source_layout)) return false;
	}
	TargetMap tm;
	int source_layer_size = source_layout.CollectTargets(&tm);
	if (source_layer_size <= 0) {
		return false;
	}

	boost::scoped_array<double> data(new double[source_layer_size]);
	FILE *fp = fopen(source_data_file, "rb");
	if (!fp) {
		perror(source_data_file);
		return false;
	}
	if (fread(data.get(), sizeof(double), source_layer_size, fp) != static_cast<size_t>(source_layer_size)) {
		cerr << "could not read data with size: " << source_layer_size << endl;
		return false;
	}
	fclose(fp);

	boost::scoped_array<char> format;
	TargetValueMap tvm;
	{
		// check model's format
		{
			FormatLoader loader(db);
			format.reset(loader.Load());
			if (!format) return false;
		}
		{
			TargetLoader loader(db);
			if (!loader.Load(tm, data.get(), &tvm))
				return false;
		}
	}

	fp = fopen(target_data_file, "r+b");
	if (!fp) {
		perror(target_data_file);
		return false;
	}
	TargetLayout target_layout;
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(target_layout_file));
		if (!loader->Load(&target_layout)) return false;
	}
	if (!target_layout.Rewrite(format.get(), tvm, fp)) {
		return false;
	}
	fclose(fp);

	return true;
}

}
