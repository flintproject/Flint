/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "reach.hh"

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>

#include <boost/functional/hash.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/input-port-loader.h"
#include "db/output-port-loader.h"
#include "db/reach-driver.h"
#include "db/scope-loader.h"
#include "db/span-loader.h"
#include "reduction.hh"

using std::cerr;
using std::endl;
using std::make_pair;
using std::multimap;
using std::set;
using std::string;
using std::pair;

namespace flint {
namespace phml {
namespace {

class Scope {
public:
	Scope(const Scope &) = delete;
	Scope &operator=(const Scope &) = delete;

	Scope(boost::uuids::uuid uuid, boost::uuids::uuid module_id)
		: uuid_(uuid),
		  module_id_(module_id),
		  label_()
	{
	}

	Scope(boost::uuids::uuid uuid, boost::uuids::uuid module_id, string label)
		: uuid_(uuid),
		  module_id_(module_id),
		  label_(label)
	{
	}

	const boost::uuids::uuid &uuid() const {return uuid_;}
	const boost::uuids::uuid &module_id() const {return module_id_;}
	const string &label() const {return label_;}

	// comparison wrt module_id_ & uuid_ (but not label_)
	bool operator<(const Scope &other) const {
		if (module_id_ < other.module_id_) return true;
		if (module_id_ == other.module_id_ &&
			uuid_ < other.uuid_) return true;
		return false;
	}

private:
	boost::uuids::uuid uuid_;
	boost::uuids::uuid module_id_;
	string label_;
};

class Port {
public:
	Port(boost::uuids::uuid module_id, int port_id, int physical_quantity_id,
		 char pq_type, Reduction reduction)
		: module_id_(module_id),
		  port_id_(port_id),
		  physical_quantity_id_(physical_quantity_id),
		  pq_type_(pq_type)
		, reduction_(reduction)
	{
	}

	const boost::uuids::uuid &module_id() const {return module_id_;}
	int port_id() const {return port_id_;}
	int physical_quantity_id() const {return physical_quantity_id_;}
	char pq_type() const {return pq_type_;}
	Reduction reduction() const {return reduction_;}

private:
	boost::uuids::uuid module_id_;
	int port_id_;
	int physical_quantity_id_;
	char pq_type_;
	Reduction reduction_;
};

class Node {
public:
	Node(boost::uuids::uuid uuid, int port_id) : uuid_(uuid), port_id_(port_id) {}

	const boost::uuids::uuid &uuid() const {return uuid_;}
	int port_id() const {return port_id_;}

	bool operator<(const Node &other) const {
		if (uuid_ < other.uuid_) return true;
		if (uuid_ == other.uuid_ &&
			port_id_ < other.port_id_) return true;
		return false;
	}

private:
	boost::uuids::uuid uuid_;
	int port_id_;
};

class InputPortHandler {
public:
	InputPortHandler(const InputPortHandler &) = delete;
	InputPortHandler &operator=(const InputPortHandler &) = delete;

	explicit InputPortHandler(std::multimap<boost::uuids::uuid, Port> *ports)
		: ports_(ports)
	{
	}

	bool Handle(boost::uuids::uuid uuid, int port_id, int pq_id,
				char pq_type, Reduction reduction)
	{
		ports_->emplace(std::make_pair(uuid, Port(uuid, port_id, pq_id, pq_type, reduction)));
		return true;
	}

private:
	std::multimap<boost::uuids::uuid, Port> *ports_;
};

bool LoadInputPorts(sqlite3 *db, std::multimap<boost::uuids::uuid, Port> *ports)
{
	std::unique_ptr<db::InputPortLoader> loader(new db::InputPortLoader(db));
	std::unique_ptr<InputPortHandler> handler(new InputPortHandler(ports));
	return loader->Load(handler.get());
}

class OutputPortHandler {
public:
	OutputPortHandler(const OutputPortHandler &) = delete;
	OutputPortHandler &operator=(const OutputPortHandler &) = delete;

	explicit OutputPortHandler(std::map<Node, Port> *ports)
		: ports_(ports)
	{
	}

	bool Handle(boost::uuids::uuid uuid, int port_id, int pq_id, char pq_type) {
		Node node(uuid, port_id);
		// reduction makes little sense for output port, so just call it unspecified
		ports_->emplace(std::make_pair(node, Port(uuid, port_id, pq_id, pq_type, Reduction::kUnspecified)));
		return true;
	}

private:
	std::map<Node, Port> *ports_;
};

bool LoadOutputPorts(sqlite3 *db, std::map<Node, Port> *ports)
{
	std::unique_ptr<db::OutputPortLoader> loader(new db::OutputPortLoader(db));
	std::unique_ptr<OutputPortHandler> handler(new OutputPortHandler(ports));
	return loader->Load(handler.get());
}

typedef std::set<Scope> ScopeSet;
typedef multimap<boost::uuids::uuid, ScopeSet::iterator> Mmap;
typedef std::unordered_map<boost::uuids::uuid,
						   ScopeSet::iterator,
						   boost::hash<boost::uuids::uuid>
						   > Umap;

class ScopeHandler {
public:
	ScopeHandler(const ScopeHandler &) = delete;
	ScopeHandler &operator=(const ScopeHandler &) = delete;

	ScopeHandler(ScopeSet *scopes, Mmap *mmap, Umap *umap)
		: scopes_(scopes),
		  mmap_(mmap),
		  umap_(umap)
	{}

	bool Handle(boost::uuids::uuid uuid, boost::uuids::uuid space_id, const char *label) {
		pair<ScopeSet::iterator, bool> p;
		if (label) {
			p = scopes_->emplace(uuid, space_id, string(label));
		} else {
			p = scopes_->emplace(uuid, space_id);
		}
		if (!p.second) {
			cerr << "duplicate entry of scope: " << uuid << endl;
			return false;
		}
		mmap_->insert(make_pair(space_id, p.first));
		umap_->insert(make_pair(uuid, p.first));
		return true;
	}

private:
	ScopeSet *scopes_;
	Mmap *mmap_;
	Umap *umap_;
};

bool LoadScopes(sqlite3 *db, ScopeSet *scopes, Mmap *mmap, Umap *umap)
{
	std::unique_ptr<db::ScopeLoader> loader(new db::ScopeLoader(db));
	std::unique_ptr<ScopeHandler> handler(new ScopeHandler(scopes, mmap, umap));
	return loader->Load(handler.get());
}

class SpanHandler {
public:
	SpanHandler(const SpanHandler &) = delete;
	SpanHandler &operator=(const SpanHandler &) = delete;

	explicit SpanHandler(multimap<Node, Node> *spans) : spans_(spans) {}

	bool Handle(boost::uuids::uuid tail_uuid, int tail_port_id,
				boost::uuids::uuid head_uuid, int head_port_id) {
		// reverse direction: from head to tail
		spans_->insert(make_pair(Node(head_uuid, head_port_id),
								 Node(tail_uuid, tail_port_id)));
		return true;
	}

private:
	multimap<Node, Node> *spans_;
};

bool LoadEdges(sqlite3 *db, multimap<Node, Node> *edges)
{
	std::unique_ptr<db::SpanLoader> loader(new db::SpanLoader(db));
	std::unique_ptr<SpanHandler> handler(new SpanHandler(edges));
	return loader->Load(handler.get());
}

void Lookup(const Node &p, multimap<Node, Node> &edges, set<Node> *q)
{
	pair<multimap<Node, Node>::iterator, multimap<Node, Node>::iterator> r;
	r = edges.equal_range(p);
	if (r.first == r.second) {
		q->insert(p);
		return;
	}
	set<Node> s;
	for (multimap<Node, Node>::iterator it=r.first;it!=r.second;++it) {
		const Node &tail = it->second;
		s.insert(tail);
	}
	for (set<Node>::const_iterator it=s.begin();it!=s.end();++it) {
		Lookup(*it, edges, q);
	}
}

void PrintIncompatiblePorts(const Port &from, const Port &to)
{
	cerr << "  from" << endl
		 << "    port-id: " << from.port_id() << endl
		 << "    module-id: " << from.module_id() << endl
		 << "  to" << endl
		 << "    port-id: " << to.port_id() << endl
		 << "    module-id: " << to.module_id() << endl;
}

} // namespace

bool Reach(sqlite3 *db)
{
	std::multimap<boost::uuids::uuid, Port> inports;
	if (!LoadInputPorts(db, &inports)) return false;

	std::map<Node, Port> outports;
	if (!LoadOutputPorts(db, &outports)) return false;

	ScopeSet scopes;
	Mmap mmap;
	Umap umap;
	if (!LoadScopes(db, &scopes, &mmap, &umap)) return false;

	multimap<Node, Node> edges;
	if (!LoadEdges(db, &edges)) return false;

	std::unique_ptr<db::ReachDriver> driver(new db::ReachDriver(db));
	// trace from each input-port
	for (auto it=inports.cbegin();it!=inports.cend();++it) {
		const boost::uuids::uuid &module_id = it->first;
		const Port &inport = it->second;

		pair<Mmap::iterator, Mmap::iterator> r;
		r = mmap.equal_range(module_id);
		for (Mmap::iterator rit=r.first;rit!=r.second;++rit) {
			const boost::uuids::uuid &uuid = rit->second->uuid();
			Node p(uuid, inport.port_id());
			set<Node> t;
			Lookup(p, edges, &t);
			for (set<Node>::const_iterator tit=t.begin();tit!=t.end();++tit) {
				const Node &o = *tit;
				if (o.port_id() <= 0) {
					cerr << "invalid port-id of edge: " << o.port_id() << endl;
					return false;
				}
				// find corresponding output-port
				Umap::const_iterator uit = umap.find(o.uuid());
				if (uit == umap.end()) {
					cerr << "missing node with uuid: " << o.uuid() << endl;
					return false;
				}
				const boost::uuids::uuid &om = uit->second->module_id();
				Node ok(om, o.port_id());
				std::map<Node, Port>::const_iterator oit = outports.find(ok);
				if (oit == outports.end()) {
					cerr << "lost without corresponding edge or output-port" << endl;
					cerr << "  port-id: " << o.port_id() << endl;
					cerr << "  module-id: " << uit->second->module_id() << endl;
					cerr << "  uuid: " << uit->second->uuid() << endl;
					if (!uit->second->label().empty()) cerr << "  label: " <<  uit->second->label() << endl;
					return false;
				}
				// check if PQ types of both sides are compatible
				if (inport.pq_type() == 's') {
					switch (oit->second.pq_type()) {
					case 'x':
						cerr << "found invalid edge from a state to a static-parameter" << endl;
						PrintIncompatiblePorts(inport, oit->second);
						return false;
					case 'v':
						cerr << "found invalid edge from a variable-parameter to a static-parameter" << endl;
						PrintIncompatiblePorts(inport, oit->second);
						return false;
					}
				}
				if (!driver->Save(tit->uuid(), oit->second.physical_quantity_id(),
								  uuid, inport.physical_quantity_id(),
								  inport.reduction()))
					return false;
			}
		}
	}
	return true;
}

}
}
