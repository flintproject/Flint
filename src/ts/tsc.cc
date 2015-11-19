/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "ts.hh"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/functional/hash.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "csv/export.h"
#include "db/driver.hh"
#include "db/eq-inserter.h"
#include "db/name_loader.h"
#include "db/query.h"
#include "db/timeseries-loader.h"
#include "db/tsref-loader.h"
#include "isdf/reader.h"
#include "utf8path.h"
#include "variable.h"

using std::cerr;
using std::endl;
using std::make_pair;
using std::strcmp;
using std::string;

namespace flint {
namespace ts {

namespace {

typedef std::unordered_map<boost::uuids::uuid,
						   std::unordered_map<int, string>,
						   boost::hash<boost::uuids::uuid> > PqMap;

class PqHandler {
public:
	PqHandler(const PqHandler &) = delete;
	PqHandler &operator=(const PqHandler &) = delete;

	explicit PqHandler(PqMap *map) : map_(map) {
		assert(map);
	}

	bool Handle(const boost::uuids::uuid &uuid, std::unique_ptr<Variable> &&var) {
		if (var->type() != 't') return true; // skip other types
		(*map_)[uuid].insert(std::make_pair(var->id(), var->name()));
		return true;
	}

private:
	PqMap *map_;
};

typedef std::set<boost::filesystem::path> PathSet;
typedef std::unordered_map<boost::uuids::uuid,
						   std::unordered_map<int, PathSet::iterator>,
						   boost::hash<boost::uuids::uuid> > TimeseriesMap;

class TimeseriesHandler {
public:
	TimeseriesHandler(const TimeseriesHandler &) = delete;
	TimeseriesHandler &operator=(const TimeseriesHandler &) = delete;

	TimeseriesHandler(PathSet *ps, TimeseriesMap *tm)
		: ps_(ps),
		  tm_(tm)
	{}

	bool Handle(const boost::uuids::uuid &uuid, int ts_id, const char *format, const char *ref) {
		boost::filesystem::path ref_path(GetPathFromUtf8(ref));
		if (strcmp(format, "csv") == 0) {
			boost::system::error_code ec;
			boost::filesystem::path temp_path("tsc.%%%%-%%%%-%%%%-%%%%.isd");
			boost::filesystem::path isd_path = boost::filesystem::unique_path(temp_path, ec);
			if (ec) {
				cerr << ec << endl;
				return false;
			}
			if (boost::filesystem::exists(isd_path, ec)) {
				cerr << "failed to create temporary path: " << isd_path << endl;
				return false;
			}
			boost::filesystem::path a_path = boost::filesystem::absolute(isd_path);
			if (!ExportIsdFromCsv(ref_path, a_path)) return false;
			PathSet::iterator it = ps_->insert(a_path).first;
			(*tm_)[uuid].insert(make_pair(ts_id, it));
			return true;
		}
		if (strcmp(format, "isd") != 0) {
			cerr << "unknown format: " << format << endl;
			return false;
		}
		PathSet::iterator it = ps_->insert(ref_path).first;
		(*tm_)[uuid].insert(make_pair(ts_id, it));
		return true;
	}

private:
	PathSet *ps_;
	TimeseriesMap *tm_;
};

class TsfilesInserter : db::StatementDriver {
public:
	explicit TsfilesInserter(sqlite3 *db)
		: db::StatementDriver(db, "INSERT INTO tsfiles VALUES (?)")
	{}

	bool Insert(const boost::filesystem::path &path)
	{
		std::unique_ptr<char[]> filename(GetUtf8FromPath(path));
		int e;
		e = sqlite3_bind_text(stmt(), 1, filename.get(), -1, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to bind filename: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			cerr << "failed to step: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

typedef std::unordered_map<string, std::uint32_t> ColumnMap;

class DescriptionHandler {
public:
	DescriptionHandler(const DescriptionHandler &) = delete;
	DescriptionHandler &operator=(const DescriptionHandler &) = delete;

	explicit DescriptionHandler(ColumnMap *cm) : cm_(cm) {}

	void GetDescription(std::uint32_t i, std::uint32_t bytes, const char *d) {
		if (!cm_->insert(make_pair(string(d, bytes), i)).second) {
			cerr << "found duplicate name of columns: " << endl;
		}
	}

private:
	ColumnMap *cm_;
};

class IsdfLoader {
public:
	IsdfLoader(const IsdfLoader &) = delete;
	IsdfLoader &operator=(const IsdfLoader &) = delete;

	explicit IsdfLoader(const boost::filesystem::path &path)
		: path_(path)
		, ifs_(path, std::ios::in|std::ios::binary)
	{}

	~IsdfLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	bool Load(ColumnMap *cm) {
		if (!ifs_.is_open()) {
			cerr << "failed to open ISDF file: "
				 << path_
				 << endl;
			return false;
		}
		isdf::Reader reader;
		if (!reader.ReadHeader(&ifs_)) return false;
		if (!reader.SkipComment(&ifs_)) return false;
		DescriptionHandler handler(cm);
		return reader.ReadDescriptions(handler, &ifs_);
	}

private:
	boost::filesystem::path path_;
	boost::filesystem::ifstream ifs_;
};

typedef std::unordered_map<boost::filesystem::path, std::unique_ptr<ColumnMap>,
						   boost::hash<boost::filesystem::path> > IsdfMap;

class TsrefHandler : db::EqInserter {
public:
	TsrefHandler(sqlite3 *db, PqMap *pqm, PathSet *ps, TimeseriesMap *tm, IsdfMap *im)
		: db::EqInserter("tscs", db)
		, pqm_(pqm)
		, ps_(ps)
		, tm_(tm)
		, im_(im)
	{}

	bool Handle(const boost::uuids::uuid &uuid, int pq_id, int ts_id, const char *element_id) {
		TimeseriesMap::const_iterator it = tm_->find(uuid);
		if (it == tm_->end()) {
			cerr << "missing module-id in timeseries: " << uuid << endl;
			return false;
		}
		std::unordered_map<int, PathSet::iterator>::const_iterator mit = it->second.find(ts_id);
		if (mit == it->second.end()) {
			cerr << "missing timeseries: "
				 << uuid
				 << " "
				 << ts_id
				 << endl;
			return false;
		}
		PqMap::const_iterator pit = pqm_->find(uuid);
		if (pit == pqm_->end()) {
			cerr << "missing module-id in physical-quantities: "
				 << uuid
				 << endl;
			return false;
		}
		std::unordered_map<int, string>::const_iterator qit = pit->second.find(pq_id);
		if (qit == pit->second.end()) {
			cerr << "missing physical-quantity-id: "
				 << uuid
				 << " "
				 << pq_id
				 << endl;
			return false;
		}
		IsdfMap::const_iterator iit = im_->find(*mit->second);
		if (iit == im_->end()) {
			cerr << "missing path: " << *mit->second << endl;
			return false;
		}
		string name(element_id);
		ColumnMap::const_iterator cit = iit->second->find(name);
		if (cit == iit->second->end()) {
			cerr << "missing element in "
				 << *mit->second
				 << " with element_id "
				 << element_id
				 << endl;
			return false;
		}
		int idx0 = static_cast<int>(std::distance(ps_->begin(), mit->second));
		int idx1 = static_cast<int>(cit->second);

		std::ostringstream oss;
		oss << "(eq %"
			<< qit->second.c_str()
			<< " ($At "
			<< idx0
			<< " "
			<< idx1
			<< " %time))";
		std::string math = oss.str();
		return Insert(uuid, math.c_str());
	}

private:
	PqMap *pqm_;
	PathSet *ps_;
	TimeseriesMap *tm_;
	IsdfMap *im_;
};

}

bool Tsc(sqlite3 *db)
{
	std::unique_ptr<PqMap> pqm(new PqMap);
	{
		std::unique_ptr<db::NameLoader> loader(new db::NameLoader(db));
		std::unique_ptr<PqHandler> handler(new PqHandler(pqm.get()));
		if (!loader->Load(handler.get())) return false;
	}
	std::unique_ptr<PathSet> ps(new PathSet);
	std::unique_ptr<TimeseriesMap> tm(new TimeseriesMap);
	{
		std::unique_ptr<db::TimeseriesLoader> loader(new db::TimeseriesLoader(db));
		std::unique_ptr<TimeseriesHandler> handler(new TimeseriesHandler(ps.get(), tm.get()));
		if (!loader->Load(handler.get())) return false;
	}
	if (!BeginTransaction(db))
		return false;
	std::unique_ptr<IsdfMap> im(new IsdfMap);
	{
		TsfilesInserter ti(db);
		for (PathSet::const_iterator it=ps->begin();it!=ps->end();++it) {
			boost::filesystem::path p = *it;
			if (!ti.Insert(p)) return false;

			std::unique_ptr<ColumnMap> cm(new ColumnMap);
			IsdfLoader loader(p);
			if (!loader.Load(cm.get()))
				return false;
			im->insert(std::make_pair(p, std::move(cm)));
		}
	}
	{
		std::unique_ptr<db::TsrefLoader> loader(new db::TsrefLoader(db));
		std::unique_ptr<TsrefHandler> handler(new TsrefHandler(db, pqm.get(), ps.get(), tm.get(), im.get()));
		if (!loader->Load(handler.get())) {
			return false;
		}
	}
	return CommitTransaction(db);
}

}
}
