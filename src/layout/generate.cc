/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "layout.hh"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/functional/hash.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/uuid/uuid.hpp>

#include "lo.pb.h"

#include "bc/pack.h"
#include "db/name_loader.h"
#include "db/space_loader.h"
#include "db/statement-driver.hh"

using std::cerr;
using std::endl;
using std::make_pair;
using std::memcpy;
using std::string;

namespace flint {
namespace layout {

namespace {

typedef std::unordered_map<boost::uuids::uuid,
						   string,
						   boost::hash<boost::uuids::uuid>
						   > ModuleMap;

class ModuleHandler {
public:
	ModuleHandler(const ModuleHandler &) = delete;
	ModuleHandler &operator=(const ModuleHandler &) = delete;

	explicit ModuleHandler(ModuleMap *mm) : mm_(mm) {}

	bool Handle(boost::uuids::uuid u, const char *name) {
		mm_->insert(make_pair(u, name));
		return true;
	}

private:
	ModuleMap *mm_;
};

class Name {
public:
	Name(const Name &) = delete;
	Name &operator=(const Name &) = delete;

	Name(char type, int id, const char *name, const char *unit, double capacity)
		: type_(type),
		  id_(id),
		  name_(name),
		  unit_(unit),
		  capacity_(capacity) {
		assert(unit);
	}

	char type() const {return type_;}
	int id() const {return id_;}
	const string &name() const {return name_;}
	const string &unit() const {return unit_;}
	double capacity() const {return capacity_;}

private:
	char type_;
	int id_;
	string name_;
	string unit_;
	double capacity_;
};

typedef std::unordered_map<boost::uuids::uuid,
						   std::vector<std::unique_ptr<Name> >,
						   boost::hash<boost::uuids::uuid>
						   > NameMap;

class NameHandler {
public:
	NameHandler(const NameHandler &) = delete;
	NameHandler &operator=(const NameHandler &) = delete;

	explicit NameHandler(NameMap *nm)
	: nm_(nm)
	{}

	bool Handle(boost::uuids::uuid u, char type, int id, const char *name, const char *unit, double capacity) {
		(*nm_)[u].emplace_back(new Name(type, id, name, unit, capacity));
		return true;
	}

private:
	NameMap *nm_;
};

class Node {
public:
	Node(const Node &) = delete;
	Node &operator=(const Node &) = delete;

	explicit Node(boost::uuids::uuid uuid) : uuid_(uuid), label_() {}
	Node(boost::uuids::uuid uuid, const string &label) : uuid_(uuid), label_(label) {}

	const boost::uuids::uuid &uuid() const {return uuid_;}
	const string &label() const {return label_;}

private:
	boost::uuids::uuid uuid_;
	string label_;
};

typedef std::vector<std::unique_ptr<Node> > NodeVector;

typedef boost::ptr_map<boost::uuids::uuid, NodeVector> TMap;

class TreeLoader : db::StatementDriver {
public:
	// Note that db is for read only.
	explicit TreeLoader(sqlite3 *db)
		: db::StatementDriver(db, "SELECT * FROM scopes")
	{
	}

	bool Load(TMap *m) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *uuid = sqlite3_column_blob(stmt(), 0);
			const void *space_id = sqlite3_column_blob(stmt(), 1);
			const unsigned char *label = sqlite3_column_text(stmt(), 2);
			assert(uuid);
			assert(space_id);
			boost::uuids::uuid u, key;
			memcpy(&u, uuid, u.size());
			memcpy(&key, space_id, key.size());
			if (label) {
				(*m)[key].emplace_back(new Node(u, string((const char *)label)));
			} else {
				(*m)[key].emplace_back(new Node(u));
			}
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

}

bool Generate(sqlite3 *db, const char *filename)
{
	std::unique_ptr<ModuleMap> mm(new ModuleMap);
	{
		db::SpaceLoader loader(db);
		std::unique_ptr<ModuleHandler> handler(new ModuleHandler(mm.get()));
		if (!loader.Load(handler.get())) return false;
	}

	std::unique_ptr<NameMap> nm(new NameMap);
	{
		db::NameLoader loader(db);
		std::unique_ptr<NameHandler> handler(new NameHandler(nm.get()));
		if (!loader.Load(handler.get())) return false;
	}

	TMap tm;
	{
		TreeLoader loader(db);
		if (!loader.Load(&tm)) return false;
	}

	std::ofstream ofs(filename, std::ios::out|std::ios::binary);
	if (!ofs.is_open()) {
		cerr << "could not open output file: " << filename << endl;
		return false;
	}

	std::unique_ptr<lo::Track> track(new lo::Track);
	std::unique_ptr<lo::Sector> sector(new lo::Sector);
	std::unique_ptr<lo::Data> data(new lo::Data);
	std::unique_ptr<char[]> tbu(new char[boost::uuids::uuid::static_size()]);
	std::unique_ptr<char[]> sbu(new char[boost::uuids::uuid::static_size()]);
	for (TMap::const_iterator it=tm.begin();it!=tm.end();++it) {
		const boost::uuids::uuid &tu(it->first);
		NameMap::const_iterator nmit = nm->find(tu);
		if (nmit == nm->end()) {
			// this is a non-functional
			continue;
		}

		std::copy(tu.begin(), tu.end(), tbu.get());
		track->set_id(tbu.get(), boost::uuids::uuid::static_size());
		track->set_nos(static_cast<int>(it->second->size()));
		track->set_nod(static_cast<int>(nmit->second.size()));

		ModuleMap::const_iterator mmit = mm->find(tu);
		if (mmit == mm->end()) {
			track->clear_name();
		} else {
			track->set_name(mmit->second);
		}

		if (!PackToOstream(*track, &ofs)) {
			return false;
		}

		for (const auto &np : nmit->second) {
			data->set_id(np->id());
			data->set_name(np->name());
			// TODO
			data->set_size(1);
			switch (np->type()) {
			case 's': data->set_type(lo::S); break;
			case 't': data->set_type(lo::T); break;
			case 'v': data->set_type(lo::V); break;
			case 'x': data->set_type(lo::X); break;
			default: assert(false); break;
			}
			data->set_unit(np->unit());
			if (np->capacity() > 0) data->set_capacity(np->capacity());
			if (!PackToOstream(*data, &ofs)) {
				return false;
			}
		}

		for (const auto &np : *it->second) {
			std::copy(np->uuid().begin(), np->uuid().end(), sbu.get());
			sector->set_id(sbu.get(), boost::uuids::uuid::static_size());
			if (np->label().empty()) {
				sector->clear_label();
			} else {
				sector->set_label(np->label());
			}
			if (!PackToOstream(*sector, &ofs)) {
				return false;
			}
		}
	}

	ofs.close();
	return true;
}

}
}
