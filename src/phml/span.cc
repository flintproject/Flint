/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "span.hh"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <vector>

#include <boost/ptr_container/ptr_set.hpp>
#include <boost/uuid/uuid_io.hpp>

using std::atoi;
using std::cerr;
using std::endl;
using std::make_pair;
using std::memcpy;
using std::multimap;
using std::vector;
using std::pair;

namespace flint {
namespace phml {
namespace {

class Edge {
public:
	Edge(const Edge &) = delete;
	Edge &operator=(const Edge &) = delete;

	Edge(const boost::uuids::uuid &tail_uuid, int tail_port_id,
		 const boost::uuids::uuid &head_uuid, int head_port_id)
		: tail_uuid_(tail_uuid),
		  tail_port_id_(tail_port_id),
		  head_uuid_(head_uuid),
		  head_port_id_(head_port_id)
	{
		assert(tail_port_id > 0);
		assert(head_port_id > 0);
	}

	const boost::uuids::uuid &tail_uuid() const {return tail_uuid_;}
	int tail_port_id() const {return tail_port_id_;}
	const boost::uuids::uuid &head_uuid() const {return head_uuid_;}
	int head_port_id() const {return head_port_id_;}

	Edge *Clone() const {
		return new Edge(tail_uuid_, tail_port_id_,
						head_uuid_, head_port_id_);
	}

	bool operator<(const Edge &other) const {
		if (tail_uuid_ < other.tail_uuid_) return true;
		if (tail_uuid_ == other.tail_uuid_ &&
			tail_port_id_ < other.tail_port_id_) return true;
		if (tail_uuid_ == other.tail_uuid_ &&
			tail_port_id_ == other.tail_port_id_ &&
			head_uuid_ < other.head_uuid_) return true;
		if (tail_uuid_ == other.tail_uuid_ &&
			tail_port_id_ == other.tail_port_id_ &&
			head_uuid_ == other.head_uuid_ &&
			head_port_id_ < other.head_port_id_) return true;
		return false;
	}

private:
	boost::uuids::uuid tail_uuid_;
	int tail_port_id_;
	boost::uuids::uuid head_uuid_;
	int head_port_id_;
};

class InstanceEdge {
public:
	InstanceEdge(const InstanceEdge &) = delete;
	InstanceEdge &operator=(const InstanceEdge &) = delete;

	InstanceEdge(size_t tail_index, int tail_port_id,
				 size_t head_index, int head_port_id)
		: tail_index_(tail_index),
		  tail_port_id_(tail_port_id),
		  head_index_(head_index),
		  head_port_id_(head_port_id)
	{
		assert(tail_port_id > 0);
		assert(head_port_id > 0);
	}

	size_t tail_index() const {return tail_index_;}
	int tail_port_id() const {return tail_port_id_;}
	size_t head_index() const {return head_index_;}
	int head_port_id() const {return head_port_id_;}

	bool operator<(const InstanceEdge &other) const {
		if (tail_index_ < other.tail_index_) return true;
		if (tail_index_ == other.tail_index_ &&
			tail_port_id_ < other.tail_port_id_) return true;
		if (tail_index_ == other.tail_index_ &&
			tail_port_id_ == other.tail_port_id_ &&
			head_index_ < other.head_index_) return true;
		if (tail_index_ == other.tail_index_ &&
			tail_port_id_ == other.tail_port_id_ &&
			head_index_ == other.head_index_ &&
			head_port_id_ < other.head_port_id_) return true;
		return false;
	}

private:
	size_t tail_index_;
	int tail_port_id_;
	size_t head_index_;
	int head_port_id_;
};

void AddEdge(boost::ptr_set<Edge> *edge_set, Edge *edge)
{
	pair<boost::ptr_set<Edge>::iterator, bool> p = edge_set->insert(edge);
	if (!p.second) {
		cerr << "warning: duplicate edge entry: "
			 << p.first->tail_uuid()
			 << ":"
			 << p.first->tail_port_id()
			 << " -> "
			 << p.first->head_uuid()
			 << ":"
			 << p.first->head_port_id()
			 << endl;
	}
}

class EdgeLoader {
public:
	EdgeLoader(const EdgeLoader &) = delete;
	EdgeLoader &operator=(const EdgeLoader &) = delete;

	explicit EdgeLoader(sqlite3 *db)
		: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db, "SELECT * FROM edges",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			exit(EXIT_FAILURE);
		}
	}

	~EdgeLoader() {
		sqlite3_finalize(stmt_);
	}

	bool Load(boost::ptr_set<Edge> *edge_set)
	{
		int e;
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const void *tail_module_id = sqlite3_column_blob(stmt_, 0);
			int tail_port_id = sqlite3_column_int(stmt_, 1);
			const void *head_module_id = sqlite3_column_blob(stmt_, 2);
			int head_port_id = sqlite3_column_int(stmt_, 3);
			assert(tail_module_id);
			assert(head_module_id);
			boost::uuids::uuid tmu, hmu;
			memcpy(&tmu, tail_module_id, tmu.size());
			memcpy(&hmu, head_module_id, hmu.size());
			AddEdge(edge_set,
					new Edge(tmu, tail_port_id,
							 hmu, head_port_id));
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	sqlite3_stmt *stmt_;
};

class JournalLoader {
public:
	JournalLoader(const JournalLoader &) = delete;
	JournalLoader &operator=(const JournalLoader &) = delete;

	explicit JournalLoader(sqlite3 *db)
		: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db, "SELECT * FROM journals",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			exit(EXIT_FAILURE);
		}
	}

	~JournalLoader() {
		sqlite3_finalize(stmt_);
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			int indent = sqlite3_column_int(stmt_, 0);
			const void *uuid = sqlite3_column_blob(stmt_, 1);
			if (!uuid) {
				cerr << "uuid is null" << endl;
				return false;
			}
			boost::uuids::uuid u;
			memcpy(&u, uuid, u.size());
			if (!handler->Handle(indent, u)) return false;
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	sqlite3_stmt *stmt_;
};

class JournalHandler {
public:
	JournalHandler(const JournalHandler &) = delete;
	JournalHandler &operator=(const JournalHandler &) = delete;

	explicit JournalHandler(boost::ptr_set<Edge> *edge_set)
		: edge_set_(edge_set),
		  instance_descendants_(),
		  instance_map_(),
		  template_descendants_()
	{
	}

	bool Handle(int indent, const boost::uuids::uuid &journal_uuid) {
		switch (indent) {
		case 3:
			instance_descendants_.push_back(journal_uuid);
			break;
		case 2:
			instance_descendants_.push_back(journal_uuid);
			{
				boost::uuids::uuid instance_id(journal_uuid);
				bool b = instance_map_.insert(std::make_pair(instance_id, std::move(instance_descendants_))).second;
				if (!b) {
					cerr << "duplicate instance id: " << instance_id << endl;
					return false;
				}
				instance_descendants_.clear();
			}
			break;
		case 1:
			template_descendants_.push_back(journal_uuid);
			break;
		case 0:
			{
				const boost::uuids::uuid &template_module_id(journal_uuid);

				multimap<boost::uuids::uuid, boost::ptr_set<Edge>::iterator> tail_map;
				multimap<boost::uuids::uuid, boost::ptr_set<Edge>::iterator> head_map;
				GenerateMaps(&tail_map, &head_map);
				// collect all edges connecting to descendants
				boost::ptr_set<Edge> inner_edges;
				for (vector<boost::uuids::uuid>::const_iterator it=template_descendants_.begin();it!=template_descendants_.end();++it) {
					const boost::uuids::uuid &uuid = *it;
					multimap<boost::uuids::uuid, boost::ptr_set<Edge>::iterator>::iterator tit = tail_map.find(uuid);
					while (tit != tail_map.end()) {
						inner_edges.insert(tit->second->Clone());
						tail_map.erase(tit);
						tit = tail_map.find(uuid);
					}
					multimap<boost::uuids::uuid, boost::ptr_set<Edge>::iterator>::iterator hit = head_map.find(uuid);
					while (hit != head_map.end()) {
						inner_edges.insert(hit->second->Clone());
						head_map.erase(hit);
						hit = head_map.find(uuid);
					}
				}

				// collect all outer edges connecting to template module
				boost::ptr_set<Edge> t_outer_edges;
				{
					multimap<boost::uuids::uuid, boost::ptr_set<Edge>::iterator>::iterator tit = tail_map.find(template_module_id);
					while (tit != tail_map.end()) {
						if (inner_edges.count(*tit->second) == 0) t_outer_edges.insert(tit->second->Clone());
						tail_map.erase(tit);
						tit = tail_map.find(template_module_id);
					}
				}
				boost::ptr_set<Edge> h_outer_edges;
				{
					multimap<boost::uuids::uuid, boost::ptr_set<Edge>::iterator>::iterator hit = head_map.find(template_module_id);
					while (hit != head_map.end()) {
						if (inner_edges.count(*hit->second) == 0) h_outer_edges.insert(hit->second->Clone());
						head_map.erase(hit);
						hit = head_map.find(template_module_id);
					}
				}

				// remove original edges
				for (boost::ptr_set<Edge>::const_iterator it=inner_edges.begin();it!=inner_edges.end();++it) {
					edge_set_->erase(*it);
				}
				for (boost::ptr_set<Edge>::const_iterator it=t_outer_edges.begin();it!=t_outer_edges.end();++it) {
					edge_set_->erase(*it);
				}
				for (boost::ptr_set<Edge>::const_iterator it=h_outer_edges.begin();it!=h_outer_edges.end();++it) {
					edge_set_->erase(*it);
				}

				// convert inner edges to instance ones
				boost::ptr_set<InstanceEdge> instance_edges;
				for (boost::ptr_set<Edge>::const_iterator it=inner_edges.begin();it!=inner_edges.end();++it) {
					size_t tail_index = 0;
					for (vector<boost::uuids::uuid>::const_iterator tit=template_descendants_.begin();tit!=template_descendants_.end();++tit) {
						if (it->tail_uuid() == *tit) break;
						tail_index++;
					}
					size_t head_index = 0;
					for (vector<boost::uuids::uuid>::const_iterator hit=template_descendants_.begin();hit!=template_descendants_.end();++hit) {
						if (it->head_uuid() == *hit) break;
						head_index++;
					}
					bool b = instance_edges.insert(new InstanceEdge(tail_index, it->tail_port_id(),
																	head_index, it->head_port_id())).second;
					assert(b);
				}

				// duplicate edges by replacing uuid of vertices
				for (auto it=instance_map_.cbegin();it!=instance_map_.cend();++it) {
					const auto &iv = it->second;
					if (iv.size() != template_descendants_.size() + 1) {
						cerr << "invalid instance descendants: " << it->first << endl;
						return false;
					}
					for (boost::ptr_set<InstanceEdge>::const_iterator iit=instance_edges.begin();iit!=instance_edges.end();++iit) {
						const InstanceEdge &ie = *iit;
						const boost::uuids::uuid &tail_uuid = iv.at(ie.tail_index());
						const boost::uuids::uuid &head_uuid = iv.at(ie.head_index());
						AddEdge(edge_set_,
								new Edge(tail_uuid, ie.tail_port_id(),
										 head_uuid, ie.head_port_id()));
					}
				}
				for (boost::ptr_set<Edge>::const_iterator it=t_outer_edges.begin();it!=t_outer_edges.end();++it) {
					const Edge &e = *it;
					for (auto iit=instance_map_.cbegin();iit!=instance_map_.cend();++iit) {
						const boost::uuids::uuid &instance_id = iit->first;
						AddEdge(edge_set_,
								new Edge(instance_id, e.tail_port_id(),
										 e.head_uuid(), e.head_port_id()));
					}
				}
				for (boost::ptr_set<Edge>::const_iterator it=h_outer_edges.begin();it!=h_outer_edges.end();++it) {
					const Edge &e = *it;
					for (auto iit=instance_map_.cbegin();iit!=instance_map_.cend();++iit) {
						const boost::uuids::uuid &instance_id = iit->first;
						AddEdge(edge_set_,
								new Edge(e.tail_uuid(), e.tail_port_id(),
										 instance_id, e.head_port_id()));
					}
				}

				instance_map_.clear();
				template_descendants_.clear();
			}
			break;
		default:
			cerr << "invalid indent of journal: " << indent << endl;
			return false;
		}
		return true;
	}

private:
	void GenerateMaps(multimap<boost::uuids::uuid, boost::ptr_set<Edge>::iterator> *tail_map,
					  multimap<boost::uuids::uuid, boost::ptr_set<Edge>::iterator> *head_map) const
	{
		tail_map->clear();
		head_map->clear();
		for (boost::ptr_set<Edge>::const_iterator it=edge_set_->begin();it!=edge_set_->end();++it) {
			tail_map->insert(make_pair(it->tail_uuid(), it));
			head_map->insert(make_pair(it->head_uuid(), it));
		}
	}

	boost::ptr_set<Edge> *edge_set_;
	std::vector<boost::uuids::uuid> instance_descendants_;
	std::map<boost::uuids::uuid, std::vector<boost::uuids::uuid> > instance_map_;
	vector<boost::uuids::uuid> template_descendants_;
};

class SpanDriver {
public:
	SpanDriver(const SpanDriver &) = delete;
	SpanDriver &operator=(const SpanDriver &) = delete;

	explicit SpanDriver(sqlite3 *db)
		: stmt_(NULL)
	{
		int e = sqlite3_prepare_v2(db, "INSERT INTO spans VALUES (?, ?, ?, ?)",
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

	~SpanDriver() {
		sqlite3_finalize(stmt_);
	}

	bool Save(const Edge &edge) {
		int e;
		e = sqlite3_bind_blob(stmt_, 1, &edge.tail_uuid(), edge.tail_uuid().size(), SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind tail_uuid: " << e << endl;
			return false;
		}
		e = sqlite3_bind_int(stmt_, 2, edge.tail_port_id());
		if (e != SQLITE_OK) {
			cerr << "failed to bind tail_port_id: " << e << endl;
			return false;
		}
		e = sqlite3_bind_blob(stmt_, 3, &edge.head_uuid(), edge.head_uuid().size(), SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind head_uuid: " << e << endl;
			return false;
		}
		e = sqlite3_bind_int(stmt_, 4, edge.head_port_id());
		if (e != SQLITE_OK) {
			cerr << "failed to bind head_port_id: " << e << endl;
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

bool Span(sqlite3 *db)
{
	boost::ptr_set<Edge> edge_set;
	{
		std::unique_ptr<EdgeLoader> loader(new EdgeLoader(db));
		if (!loader->Load(&edge_set)) return false;
	}

	// do spanning with journals
	{
		std::unique_ptr<JournalLoader> loader(new JournalLoader(db));
		std::unique_ptr<JournalHandler> handler(new JournalHandler(&edge_set));
		if (!loader->Load(handler.get())) return false;
	}

	// write into spans
	std::unique_ptr<SpanDriver> driver(new SpanDriver(db));
	for (boost::ptr_set<Edge>::const_iterator it=edge_set.begin();it!=edge_set.end();++it) {
		if (!driver->Save(*it)) return false;
	}

	return true;
}

}
}
