/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "branch.h"

#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <string>

#include <boost/cstdint.hpp>
#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_ptr.hpp>

#include "uuidgen.h"

using std::cerr;
using std::endl;
using std::strcmp;
using std::string;

namespace {

class Node : boost::noncopyable {
public:
	Node(const string &uuid, const string &module_id, size_t level) : uuid_(uuid), module_id_(module_id), level_(level), label_() {}
	Node(const string &uuid, const string &module_id, size_t level, const string &label) : uuid_(uuid), module_id_(module_id), level_(level), label_(label) {}

	const string &uuid() const {return uuid_;}
	const string &module_id() const {return module_id_;}
	size_t level() const {return level_;}
	const string &label() const {return label_;}

private:
	string uuid_;
	string module_id_;
	size_t level_;
	string label_;
};

class Instance : boost::noncopyable {
public:
	explicit Instance(const string &uuid) : uuid_(uuid), label_() {}
	Instance(const string &uuid, const string &label) : uuid_(uuid), label_(label) {}

	const string &uuid() const {return uuid_;}
	const string &label() const {return label_;}

private:
	string uuid_;
	string label_;
};

typedef boost::ptr_multimap<string, Instance> InstanceMap;

class InstanceLoader : boost::noncopyable {
public:
	explicit InstanceLoader(sqlite3 *db)
	: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db, "SELECT * FROM joins",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: "
				 << e
				 << ": "
				 << __FILE__
				 << ":"
				 << __LINE__
				 << endl;
			exit(EXIT_FAILURE);
		}
	}

	~InstanceLoader() {
		sqlite3_finalize(stmt_);
	}

	bool Load(InstanceMap *m) {
		int e;
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const unsigned char *module_id = sqlite3_column_text(stmt_, 0);
			const unsigned char *uuid = sqlite3_column_text(stmt_, 1);
			const unsigned char *label = sqlite3_column_text(stmt_, 2);

			assert(uuid);
			if (!module_id) {
				cerr << "template for instance "
					 << uuid
					 << " is unknown i.e. its template-id does not equal to any in the template-set"
					 << endl;
				return false;
			}
			string key((const char *)module_id);
			if (label) {
				m->insert(key, new Instance(string((const char *)uuid),
											string((const char *)label)));
			} else {
				m->insert(key, new Instance(string((const char *)uuid)));
			}
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt_);
		return true;
	}

private:
	sqlite3_stmt *stmt_;
};

class JournalDriver : boost::noncopyable {
public:
	explicit JournalDriver(sqlite3 *db)
		: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db, "INSERT INTO journals VALUES (?, ?)",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: "
				 << e
				 << ": "
				 << __FILE__
				 << ":"
				 << __LINE__
				 << endl;
			exit(EXIT_FAILURE);
		}
	}

	~JournalDriver() {
		sqlite3_finalize(stmt_);
	}

	bool Save(int indent, const char *uuid) {
		int e;
		e = sqlite3_bind_int(stmt_, 1, indent);
		if (e != SQLITE_OK) {
			cerr << "failed to bind indent: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt_, 2, uuid, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind uuid: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt_);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt_);
		return true;
	}

private:
	sqlite3_stmt *stmt_;
};

class NodeDriver : boost::noncopyable {
public:
	explicit NodeDriver(sqlite3 *db)
		: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db, "INSERT INTO scopes VALUES (?, ?, ?)",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: "
				 << e
				 << ": "
				 << __FILE__
				 << ":"
				 << __LINE__
				 << endl;
			exit(EXIT_FAILURE);
		}
	}

	~NodeDriver() {
		sqlite3_finalize(stmt_);
	}

	bool Save(const Node &node) {
		int e;
		e = sqlite3_bind_text(stmt_, 1, node.uuid().c_str(), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind uuid: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt_, 2, node.module_id().c_str(), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind space_id: " << e << endl;
			return false;
		}
		if (node.label().empty()) {
			e = sqlite3_bind_null(stmt_, 3);
		} else {
			e = sqlite3_bind_text(stmt_, 3, node.label().c_str(), -1, SQLITE_STATIC);
		}
		if (e != SQLITE_OK) {
			cerr << "failed to bind label: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt_);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt_);
		return true;
	}

private:
	sqlite3_stmt *stmt_;
};

} // namespace

bool Branch(const boost::filesystem::path &path, sqlite3 *db)
{
	// load instances at first
	InstanceMap instance_map;
	{
		boost::scoped_ptr<InstanceLoader> loader(new InstanceLoader(db));
		if (!loader->Load(&instance_map)) return false;
	}

	boost::scoped_ptr<UuidGenerator> gen(new UuidGenerator(path));

	boost::scoped_ptr<JournalDriver> jd(new JournalDriver(db));

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
		string uuid((const char *)sqlite3_column_text(stmt, 0));
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
			const string &instance_id = it->second->uuid();
			for (boost::ptr_vector<Node>::const_iterator vit=v.begin();vit!=v.end();++vit) {
				string u = (*gen)();
				result.push_back(new Node(u, vit->module_id(), vit->level(), it->second->label()));
				if (!jd->Save(3, u.c_str())) return false;
			}
			result.push_back(new Node(instance_id, uuid, level, it->second->label()));
			if (!jd->Save(2, instance_id.c_str())) return false;

			// search next instance of the same template
			instance_map.erase(it);
			it = instance_map.find(uuid);
		} while (it != instance_map.end());
		// write original nodes into journal
		for (boost::ptr_vector<Node>::const_iterator vit=v.begin();vit!=v.end();++vit) {
			if (!jd->Save(1, vit->uuid().c_str())) return false;
		}
		if (!jd->Save(0, uuid.c_str())) return false;
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

	boost::scoped_ptr<NodeDriver> nd(new NodeDriver(db));
	for (boost::ptr_vector<Node>::const_iterator it=result.begin();it!=result.end();++it) {
		if (!nd->Save(*it)) return false;
	}

	return true;
}
