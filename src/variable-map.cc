/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "variable-map.h"

#include "db/variable-loader.h"
#include "variable.h"

#include <cassert>

using std::cerr;
using std::endl;

namespace flint {

namespace {

class Handler {
public:
	explicit Handler(VariableMap *vm)
		: vm_(vm)
	{
	}

	bool Handle(const boost::uuids::uuid &u, std::unique_ptr<Variable> &&v) {
		vm_->Add(u, std::move(v));
		return true;
	}

private:
	VariableMap *vm_;
};

}

void VariableMap::Add(const boost::uuids::uuid &u,
					  std::unique_ptr<Variable> &&v)
{
	std::string name = v->name();
	m_[u].insert(std::make_pair(name, std::move(v)));
}

bool VariableMap::Load(sqlite3 *db)
{
	db::VariableLoader loader(db);
	Handler handler(this);
	return loader.Load(&handler);
}

const Variable *VariableMap::Find(const boost::uuids::uuid &u,
								  const std::string &name) const
{
	auto it = m_.find(u);
	if (it == m_.end())
		return nullptr;
	auto vit = it->second.find(name);
	if (vit == it->second.end())
		return nullptr;
	return vit->second.get();
}

}
