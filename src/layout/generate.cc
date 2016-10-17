/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "layout.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/functional/hash.hpp>
#include <boost/uuid/uuid.hpp>

#include "lo.pb.h"

#include "bc/pack.h"
#include "db/space_loader.h"
#include "db/statement-driver.h"
#include "db/variable-loader.h"
#include "variable.h"

using std::memcpy;

namespace flint {
namespace layout {

namespace {

typedef std::unordered_map<boost::uuids::uuid,
						   std::string,
						   boost::hash<boost::uuids::uuid>
						   > ModuleMap;

class ModuleHandler {
public:
	ModuleHandler(const ModuleHandler &) = delete;
	ModuleHandler &operator=(const ModuleHandler &) = delete;

	explicit ModuleHandler(ModuleMap *mm) : mm_(mm) {}

	bool Handle(boost::uuids::uuid u, const char *name) {
		mm_->emplace(u, name);
		return true;
	}

private:
	ModuleMap *mm_;
};

typedef std::unordered_map<boost::uuids::uuid,
						   std::vector<std::unique_ptr<Variable> >,
						   boost::hash<boost::uuids::uuid>
						   > NameMap;

class NameHandler {
public:
	NameHandler(const NameHandler &) = delete;
	NameHandler &operator=(const NameHandler &) = delete;

	explicit NameHandler(NameMap *nm)
	: nm_(nm)
	{}

	bool Handle(boost::uuids::uuid u, std::unique_ptr<Variable> &&var) {
		(*nm_)[u].emplace_back(std::move(var));
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
	Node(boost::uuids::uuid uuid, const std::string &label) : uuid_(uuid), label_(label) {}

	const boost::uuids::uuid &uuid() const {return uuid_;}
	const std::string &label() const {return label_;}

private:
	boost::uuids::uuid uuid_;
	std::string label_;
};

typedef std::vector<std::unique_ptr<Node> > NodeVector;

typedef std::map<boost::uuids::uuid, NodeVector> TMap;

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
				(*m)[key].emplace_back(new Node(u, std::string((const char *)label)));
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
		db::VariableLoader loader(db);
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
		std::cerr << "could not open output file: " << filename << std::endl;
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
		size_t nos = it->second.size();
		assert(nos > 0);
		track->set_nos(static_cast<int>(nos));
		size_t nod = nmit->second.size();
		assert(nod > 0);
		track->set_nod(static_cast<int>(nod));

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
			assert(np->col() > 0);
			assert(np->row() > 0);
			data->set_col(np->col());
			data->set_row(np->row());
			switch (np->type()) {
			case 's': data->set_type(lo::S); break;
			case 't': data->set_type(lo::T); break;
			case 'v': data->set_type(lo::V); break;
			case 'x': data->set_type(lo::X); break;
			default: assert(false); break;
			}
			data->set_unit(np->unit());
			if (np->capacity() > 0)
				data->set_capacity(np->capacity());
			else
				data->clear_capacity();
			data->set_independent(np->independent());
			if (!PackToOstream(*data, &ofs)) {
				return false;
			}
		}

		for (const auto &np : it->second) {
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
