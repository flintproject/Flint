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
#include <string>
#include <unordered_map>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/ptr_container/ptr_unordered_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_generators.hpp>

#include "lo.pb.h"

#include "bc/pack.h"
#include "db/name_loader.h"
#include "db/space_loader.h"
#include "db/statement-driver.hh"

using std::cerr;
using std::endl;
using std::make_pair;
using std::string;

namespace layout {

namespace {

typedef std::unordered_map<boost::uuids::uuid,
						   string,
						   boost::hash<boost::uuids::uuid>
						   > ModuleMap;

class ModuleHandler : boost::noncopyable {
public:
	explicit ModuleHandler(ModuleMap *mm) : mm_(mm) {}

	bool Handle(boost::uuids::uuid u, const char *name) {
		mm_->insert(make_pair(u, name));
		return true;
	}

private:
	ModuleMap *mm_;
};

class Name : boost::noncopyable {
public:
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

typedef boost::ptr_unordered_map<boost::uuids::uuid, boost::ptr_vector<Name> > NameMap;

class NameHandler : boost::noncopyable {
public:
	explicit NameHandler(NameMap *nm)
	: nm_(nm)
	{}

	bool Handle(boost::uuids::uuid u, char type, int id, const char *name, const char *unit, double capacity) {
		(*nm_)[u].push_back(new Name(type, id, name, unit, capacity));
		return true;
	}

private:
	NameMap *nm_;
};

class Node : boost::noncopyable {
public:
	explicit Node(boost::uuids::uuid uuid) : uuid_(uuid), label_() {}
	Node(boost::uuids::uuid uuid, const string &label) : uuid_(uuid), label_(label) {}

	const boost::uuids::uuid &uuid() const {return uuid_;}
	const string &label() const {return label_;}

private:
	boost::uuids::uuid uuid_;
	string label_;
};

typedef boost::ptr_vector<Node> NodeVector;

typedef boost::ptr_map<boost::uuids::uuid, NodeVector> TMap;

class TreeLoader : db::StatementDriver {
public:
	// Note that db is for read only.
	explicit TreeLoader(sqlite3 *db)
		: db::StatementDriver(db, "SELECT * FROM scopes")
		, gen_()
	{
	}

	bool Load(TMap *m) {
		static const int kUuidSize = 36;

		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const unsigned char *uuid = sqlite3_column_text(stmt(), 0);
			const unsigned char *space_id = sqlite3_column_text(stmt(), 1);
			const unsigned char *label = sqlite3_column_text(stmt(), 2);
			boost::uuids::uuid key = gen_((const char *)space_id);
			if (label) {
				(*m)[key].push_back(new Node(gen_(string((const char *)uuid, kUuidSize)), string((const char *)label)));
			} else {
				(*m)[key].push_back(new Node(gen_(string((const char *)uuid, kUuidSize))));
			}
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}

private:
	boost::uuids::string_generator gen_;
};

}

bool Generate(sqlite3 *db, const char *filename)
{
	boost::scoped_ptr<ModuleMap> mm(new ModuleMap);
	{
		db::SpaceLoader loader(db);
		boost::scoped_ptr<ModuleHandler> handler(new ModuleHandler(mm.get()));
		if (!loader.Load(handler.get())) return false;
	}

	boost::scoped_ptr<NameMap> nm(new NameMap);
	{
		db::NameLoader loader(db);
		boost::scoped_ptr<NameHandler> handler(new NameHandler(nm.get()));
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

	boost::scoped_ptr<lo::Track> track(new lo::Track);
	boost::scoped_ptr<lo::Sector> sector(new lo::Sector);
	boost::scoped_ptr<lo::Data> data(new lo::Data);
	boost::scoped_array<char> tbu(new char[16]); // 16 is UUID's size
	boost::scoped_array<char> sbu(new char[16]); // 16 is UUID's size
	for (TMap::const_iterator it=tm.begin();it!=tm.end();++it) {
		const boost::uuids::uuid &tu(it->first);
		NameMap::const_iterator nmit = nm->find(tu);
		if (nmit == nm->end()) {
			// this is a non-functional
			continue;
		}

		std::copy(tu.begin(), tu.end(), tbu.get());
		track->set_id(tbu.get(), 16);
		track->set_nos(static_cast<int>(it->second->size()));
		track->set_nod(static_cast<int>(nmit->second->size()));

		ModuleMap::const_iterator mmit = mm->find(tu);
		if (mmit == mm->end()) {
			track->clear_name();
		} else {
			track->set_name(mmit->second);
		}

		if (!PackToOstream(*track, &ofs)) {
			return false;
		}

		for (boost::ptr_vector<Name>::const_iterator nit=nmit->second->begin();nit!=nmit->second->end();++nit) {
			data->set_id(nit->id());
			data->set_name(nit->name());
			// TODO
			data->set_size(1);
			switch (nit->type()) {
			case 's': data->set_type(lo::S); break;
			case 't': data->set_type(lo::T); break;
			case 'v': data->set_type(lo::V); break;
			case 'x': data->set_type(lo::X); break;
			default: assert(false); break;
			}
			data->set_unit(nit->unit());
			if (nit->capacity() > 0) data->set_capacity(nit->capacity());
			if (!PackToOstream(*data, &ofs)) {
				return false;
			}
		}

		for (NodeVector::const_iterator sit=it->second->begin();sit!=it->second->end();++sit) {
			std::copy(sit->uuid().begin(), sit->uuid().end(), sbu.get());
			sector->set_id(sbu.get(), 16);
			if (sit->label().empty()) {
				sector->clear_label();
			} else {
				sector->set_label(sit->label());
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
