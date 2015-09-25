/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "branch.hh"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/statement-driver.hh"
#include "uuidgen.h"

using std::cerr;
using std::endl;
using std::memcpy;
using std::strcmp;
using std::string;

namespace flint {
namespace phml {
namespace {

class Node : boost::noncopyable {
public:
	Node(const boost::uuids::uuid &uuid, const boost::uuids::uuid &module_id, size_t level)
		: uuid_(uuid), module_id_(module_id), level_(level), label_()
	{}
	Node(const boost::uuids::uuid &uuid, const boost::uuids::uuid &module_id, size_t level, const string &label)
		: uuid_(uuid), module_id_(module_id), level_(level), label_(label)
	{}

	const boost::uuids::uuid &uuid() const {return uuid_;}
	const boost::uuids::uuid &module_id() const {return module_id_;}
	size_t level() const {return level_;}
	const string &label() const {return label_;}

private:
	boost::uuids::uuid uuid_;
	boost::uuids::uuid module_id_;
	size_t level_;
	string label_;
};

class Instance : boost::noncopyable {
public:
	explicit Instance(const boost::uuids::uuid &uuid)
		: uuid_(uuid), label_()
	{}
	Instance(const boost::uuids::uuid &uuid, const string &label)
		: uuid_(uuid), label_(label)
	{}

	const boost::uuids::uuid &uuid() const {return uuid_;}
	const string &label() const {return label_;}

private:
	boost::uuids::uuid uuid_;
	string label_;
};

typedef boost::ptr_multimap<boost::uuids::uuid, Instance> InstanceMap;

class InstanceLoader : public db::StatementDriver {
public:
	explicit InstanceLoader(sqlite3 *db)
		: db::StatementDriver(db, "SELECT * FROM joins")
	{
	}

	bool Load(InstanceMap *m) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *module_id = sqlite3_column_blob(stmt(), 0);
			const void *uuid = sqlite3_column_blob(stmt(), 1);
			const unsigned char *label = sqlite3_column_text(stmt(), 2);

			assert(uuid);
			boost::uuids::uuid uu;
			memcpy(&uu, uuid, uu.size());
			if (!module_id) {
				cerr << "template for instance "
					 << uu
					 << " is unknown i.e. its template-id does not equal to any in the template-set"
					 << endl;
				return false;
			}
			boost::uuids::uuid mu;
			memcpy(&mu, module_id, mu.size());
			if (label) {
				m->insert(mu, new Instance(uu, string((const char *)label)));
			} else {
				m->insert(mu, new Instance(uu));
			}
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

class JournalDriver : public db::StatementDriver {
public:
	explicit JournalDriver(sqlite3 *db)
		: db::StatementDriver(db, "INSERT INTO journals VALUES (?, ?)")
	{
	}

	bool Save(int indent, const boost::uuids::uuid &uuid) {
		int e;
		e = sqlite3_bind_int(stmt(), 1, indent);
		if (e != SQLITE_OK) {
			cerr << "failed to bind indent: " << e << endl;
			return false;
		}
		e = sqlite3_bind_blob(stmt(), 2, &uuid, uuid.size(), SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind uuid: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

class NodeDriver : public db::StatementDriver {
public:
	explicit NodeDriver(sqlite3 *db)
		: db::StatementDriver(db, "INSERT INTO scopes VALUES (?, ?, ?)")
	{
	}

	bool Save(const Node &node) {
		int e;
		e = sqlite3_bind_blob(stmt(), 1, &node.uuid(), node.uuid().size(), SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind uuid: " << e << endl;
			return false;
		}
		e = sqlite3_bind_blob(stmt(), 2, &node.module_id(), node.module_id().size(), SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind space_id: " << e << endl;
			return false;
		}
		if (node.label().empty()) {
			e = sqlite3_bind_null(stmt(), 3);
		} else {
			e = sqlite3_bind_text(stmt(), 3, node.label().c_str(), -1, SQLITE_STATIC);
		}
		if (e != SQLITE_OK) {
			cerr << "failed to bind label: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

} // namespace

bool Branch(const boost::filesystem::path &path, sqlite3 *db)
{
	// load instances at first
	InstanceMap instance_map;
	{
		std::unique_ptr<InstanceLoader> loader(new InstanceLoader(db));
		if (!loader->Load(&instance_map)) return false;
	}

	std::unique_ptr<UuidGenerator> gen(new UuidGenerator(path));

	std::unique_ptr<JournalDriver> jd(new JournalDriver(db));

	sqlite3_stmt *stmt;
	int e = sqlite3_prepare_v2(db, "SELECT t.module_id, t.level, m.template_state FROM trees AS t LEFT JOIN modules AS m ON t.module_id = m.module_id",
							   -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: "
			 << e
			 << ": "
			 << __FILE__
			 << ":"
			 << __LINE__
			 << endl;
		return false;
	}

	boost::ptr_vector<Node> result;
	boost::ptr_vector<Node> tmp;
	while ( (e = sqlite3_step(stmt)) == SQLITE_ROW ) {
		boost::uuids::uuid uuid;
		memcpy(&uuid, sqlite3_column_blob(stmt, 0), uuid.size());
		size_t level = static_cast<size_t>(sqlite3_column_int(stmt, 1));
		const unsigned char *template_state = sqlite3_column_text(stmt, 2);

		// include the module if it is a non-template module
		if (!template_state || strcmp((const char *)template_state, "true") != 0) {
			result.push_back(new Node(uuid, uuid, level));
			continue;
		}
		// search top module of the template
		InstanceMap::iterator it = instance_map.find(uuid);
		if (it == instance_map.end()) {
			tmp.push_back(new Node(uuid, uuid, level));
			continue;
		}
		// rollback and collect the module and its descendants from tree
		boost::ptr_vector<Node> v;
		while (!tmp.empty()) {
			boost::ptr_vector<Node>::auto_type node = tmp.pop_back();
			if (node->level() <= level) {
				// restore one which is not a descendant
				tmp.push_back(node.release());
				break;
			}
			v.insert(v.begin(), node.release());
		}
		// duplicate the subtree
		do {
			const boost::uuids::uuid &instance_id = it->second->uuid();
			for (boost::ptr_vector<Node>::const_iterator vit=v.begin();vit!=v.end();++vit) {
				boost::uuids::uuid u = (*gen)();
				result.push_back(new Node(u, vit->module_id(), vit->level(), it->second->label()));
				if (!jd->Save(3, u)) return false;
			}
			result.push_back(new Node(instance_id, uuid, level, it->second->label()));
			if (!jd->Save(2, instance_id)) return false;

			// search next instance of the same template
			instance_map.erase(it);
			it = instance_map.find(uuid);
		} while (it != instance_map.end());
		// write original nodes into journal
		for (boost::ptr_vector<Node>::const_iterator vit=v.begin();vit!=v.end();++vit) {
			if (!jd->Save(1, vit->uuid())) return false;
		}
		if (!jd->Save(0, uuid)) return false;
	}
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << e << endl;
		return false;
	}
	sqlite3_finalize(stmt);

	if (!instance_map.empty()) {
		cerr << "the following instances miss their templates:" << endl;
		for (InstanceMap::const_iterator it=instance_map.begin();it!=instance_map.end();++it) {
			cerr << it->first << " " << it->second->uuid() << endl;
		}
		return false;
	}

	std::unique_ptr<NodeDriver> nd(new NodeDriver(db));
	for (boost::ptr_vector<Node>::const_iterator it=result.begin();it!=result.end();++it) {
		if (!nd->Save(*it)) return false;
	}

	return true;
}

}
}
