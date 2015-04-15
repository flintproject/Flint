/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "csv/export.h"
#include "db/driver.h"
#include "db/name_loader.h"
#include "db/timeseries-loader.h"
#include "db/tsref-loader.h"
#include "isdf/reader.h"

using std::cerr;
using std::cout;
using std::endl;
using std::make_pair;
using std::map;
using std::strcmp;
using std::string;
using std::strlen;

namespace {

typedef boost::ptr_map<boost::uuids::uuid, map<int, string> > PqMap;

class PqHandler : boost::noncopyable {
public:
	explicit PqHandler(PqMap *map) : map_(map) {
		assert(map);
	}

	bool Handle(boost::uuids::uuid uuid, char type, int pq_id, const char *name, const char * /*unit*/, double /*capacity*/) {
		if (type != 't') return true; // skip other types
		(*map_)[uuid].insert(make_pair(pq_id, string(name)));
		return true;
	}

private:
	PqMap *map_;
};

typedef std::set<string> PathSet;
typedef boost::ptr_map<boost::uuids::uuid, map<int, PathSet::iterator> > TimeseriesMap;

class TimeseriesHandler : boost::noncopyable {
public:
	TimeseriesHandler(PathSet *ps, TimeseriesMap *tm)
		: ps_(ps),
		  tm_(tm)
	{}

	bool Handle(boost::uuids::uuid uuid, int ts_id, const char *format, const char *ref) {
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
			std::string a_str = a_path.string();
			if (!ExportIsdFromCsv(ref, a_str.c_str())) return false;
			PathSet::iterator it = ps_->insert(a_str).first;
			(*tm_)[uuid].insert(make_pair(ts_id, it));
			return true;
		}
		if (strcmp(format, "isd") != 0) {
			cerr << "unknown format: " << format << endl;
			return false;
		}
		string path(ref);
		PathSet::iterator it = ps_->insert(path).first;
		(*tm_)[uuid].insert(make_pair(ts_id, it));
		return true;
	}

private:
	PathSet *ps_;
	TimeseriesMap *tm_;
};

typedef map<string, boost::uint32_t> ColumnMap;

class DescriptionHandler : boost::noncopyable {
public:
	explicit DescriptionHandler(ColumnMap *cm) : cm_(cm) {}

	void GetDescription(boost::uint32_t i, boost::uint32_t bytes, const char *d) {
		if (!cm_->insert(make_pair(string(d, bytes), i)).second) {
			cerr << "found duplicate name of columns: " << endl;
		}
	}

private:
	ColumnMap *cm_;
};

class IsdfLoader : boost::noncopyable {
public:
	explicit IsdfLoader(const char *file) : file_(file), ifs_(file, std::ios::in|std::ios::binary) {}

	~IsdfLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	bool Load(ColumnMap *cm) {
		if (!ifs_.is_open()) {
			cerr << "failed to open ISDF file: "
				 << file_
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
	const char *file_;
	std::ifstream ifs_;
};

typedef boost::ptr_map<string, ColumnMap> IsdfMap;

class TsrefHandler : boost::noncopyable {
public:
	TsrefHandler(PqMap *pqm, PathSet *ps, TimeseriesMap *tm, IsdfMap *im)
		: pqm_(pqm),
		  ps_(ps),
		  tm_(tm),
		  im_(im)
	{}

	bool Handle(boost::uuids::uuid uuid, int pq_id, int ts_id, const char *element_id) {
		TimeseriesMap::const_iterator it = tm_->find(uuid);
		if (it == tm_->end()) {
			cerr << "missing module-id in timeseries: " << uuid << endl;
			return false;
		}
		map<int, PathSet::iterator>::const_iterator mit = it->second->find(ts_id);
		if (mit == it->second->end()) {
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
		map<int, string>::const_iterator qit = pit->second->find(pq_id);
		if (qit == pit->second->end()) {
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
		cout << uuid
			 << " (eq %"
			 << qit->second.c_str()
			 << " ($At "
			 << idx0
			 << " "
			 << idx1
			 << " %time))"
			 << endl;
		return true;
	}

private:
	PqMap *pqm_;
	PathSet *ps_;
	TimeseriesMap *tm_;
	IsdfMap *im_;
};

void usage()
{
	cerr << "usage: flint-tsc DB ENTRIES" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if ( argc == 2 && (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) ) {
		usage();
		return EXIT_SUCCESS;
	}
	if (argc != 3) {
		usage();
		return EXIT_FAILURE;
	}

	boost::scoped_ptr<db::Driver> driver(new db::Driver(argv[1]));
	boost::scoped_ptr<PqMap> pqm(new PqMap);
	{
		boost::scoped_ptr<db::NameLoader> loader(new db::NameLoader(driver->db()));
		boost::scoped_ptr<PqHandler> handler(new PqHandler(pqm.get()));
		if (!loader->Load(handler.get())) return EXIT_FAILURE;
	}
	boost::scoped_ptr<PathSet> ps(new PathSet);
	boost::scoped_ptr<TimeseriesMap> tm(new TimeseriesMap);
	{
		boost::scoped_ptr<db::TimeseriesLoader> loader(new db::TimeseriesLoader(driver->db()));
		boost::scoped_ptr<TimeseriesHandler> handler(new TimeseriesHandler(ps.get(), tm.get()));
		if (!loader->Load(handler.get())) return EXIT_FAILURE;
	}
	boost::scoped_ptr<IsdfMap> im(new IsdfMap);
	{
		FILE *fp = fopen(argv[2], "wb");
		if (!fp) {
			cerr << "failed to open " << argv[2] << endl;
			return EXIT_FAILURE;
		}
		for (PathSet::iterator it=ps->begin(); it!=ps->end(); ++it) {
			const char *p = it->c_str();
			size_t s = strlen(p);
			fwrite(p, s, 1, fp);
			fputc('\0', fp);

			ColumnMap *cm = new ColumnMap;
			IsdfLoader loader(p);
			if (!loader.Load(cm)) {
				delete cm;
				fclose(fp);
				return EXIT_FAILURE;
			}
			string path(p);
			im->insert(path, cm);
		}
		fclose(fp);
	}
	{
		boost::scoped_ptr<db::TsrefLoader> loader(new db::TsrefLoader(driver->db()));
		boost::scoped_ptr<TsrefHandler> handler(new TsrefHandler(pqm.get(), ps.get(), tm.get(), im.get()));
		if (!loader->Load(handler.get())) {
			return EXIT_FAILURE;
		}
	}
	return EXIT_SUCCESS;
}
