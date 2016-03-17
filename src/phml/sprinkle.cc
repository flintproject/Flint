/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "sprinkle.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include <boost/uuid/uuid_io.hpp>

#include "db/journal-loader.h"
#include "db/sprinkle-driver.h"
#include "db/target-loader.h"

using std::cerr;
using std::cout;
using std::endl;
using std::make_pair;
using std::map;
using std::pair;
using std::printf;
using std::string;
using std::vector;

namespace flint {
namespace phml {
namespace {

typedef std::map<pair<boost::uuids::uuid, boost::uuids::uuid>, std::map<int, double> > TargetMap;

class TargetHandler {
public:
	TargetHandler(const TargetHandler &) = delete;
	TargetHandler &operator=(const TargetHandler &) = delete;

	explicit TargetHandler(TargetMap *tm) : tm_(tm) {}

	bool Handle(boost::uuids::uuid u0, boost::uuids::uuid u1, int pq_id, double value) {
		(*tm_)[make_pair(u1, u0)].insert(make_pair(pq_id, value));
		return true;
	}

private:
	TargetMap *tm_;
};

typedef map<pair<boost::uuids::uuid, boost::uuids::uuid>, boost::uuids::uuid> JournalMap;

class JournalHandler {
public:
	JournalHandler(const JournalHandler &) = delete;
	JournalHandler &operator=(const JournalHandler &) = delete;

	explicit JournalHandler(JournalMap *jm) : jm_(jm), instances_(new vector<boost::uuids::uuid>) {}

	bool Handle(int n, boost::uuids::uuid u) {
		switch (n) {
		case 3:
			instances_->push_back(u);
			break;
		case 2:
			{
				bool b = im_.insert(make_pair(u, std::move(instances_))).second;
				if (!b) {
					cerr << "duplicate instance id: " << u << endl;
					return false;
				}
				instances_.reset(new vector<boost::uuids::uuid>);
			}
			break;
		case 1:
			templates_.push_back(u);
			break;
		case 0:
			{
				size_t s = templates_.size();
				for (auto imit=im_.cbegin();imit!=im_.cend();++imit) {
					if (imit->second->size() != s) {
						cerr << "mismatch of numbers of template/instance: " << u << endl;
						return false;
					}
					for (size_t i=0;i<s;i++) {
						bool b = jm_->insert(make_pair(make_pair(templates_[i], imit->first),
													   (*imit->second)[i])).second;
						if (!b) {
							cerr << "duplicate template/instance: " << templates_[i]
								 << "/" << imit->first
								 << endl;
							return false;
						}
					}
				}
				im_.clear();
				templates_.clear();
			}
			break;
		default:
			assert(false);
			break;
		}
		return true;
	}

private:
	JournalMap *jm_;
	std::unique_ptr<vector<boost::uuids::uuid> > instances_;
	std::map<boost::uuids::uuid, std::unique_ptr<vector<boost::uuids::uuid> > > im_;
	vector<boost::uuids::uuid> templates_;
};

} // namespace

bool Sprinkle(sqlite3 *db)
{
	// load target
	std::unique_ptr<TargetMap> tm(new TargetMap);
	{
		std::unique_ptr<db::TargetLoader> loader(new db::TargetLoader(db));
		std::unique_ptr<TargetHandler> handler(new TargetHandler(tm.get()));
		if (!loader->Load(handler.get())) {
			return false;
		}
	}

	// load journal
	std::unique_ptr<JournalMap> jm(new JournalMap);
	{
		std::unique_ptr<db::JournalLoader> loader(new db::JournalLoader(db));
		std::unique_ptr<JournalHandler> handler(new JournalHandler(jm.get()));
		if (!loader->Load(handler.get())) {
			return false;
		}
	}

	std::unique_ptr<db::SprinkleDriver> driver(new db::SprinkleDriver(db));
	for (auto it=tm->cbegin();it!=tm->cend();++it) {
		JournalMap::const_iterator jmit = jm->find(it->first);
		if (jmit == jm->end()) {
			cerr << "unknown template/instance: " << it->first.first
				 << "/" << it->first.second << endl;
			return false;
		}
		boost::uuids::uuid track_id = it->first.first;
		boost::uuids::uuid sector_id = jmit->second;
		for (auto dit=it->second.cbegin();dit!=it->second.cend();++dit) {
			if (!driver->Save(track_id, sector_id, dit->first, dit->second)) return false;
		}
	}

	return true;
}

}
}
