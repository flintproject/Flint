/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "gui/param-tree.h"

namespace flint {
namespace gui {

ParamTreeNode::~ParamTreeNode() = default;

ParamTree::~ParamTree() = default;

void ParamTree::Import(const std::unordered_set<std::unique_ptr<ParamTreeNode> > &given_nodes,
					   const std::unordered_map<std::uintptr_t, std::uintptr_t> &given_pm)
{
	pm.clear();
	nodes.clear();

	std::unordered_map<std::uintptr_t, std::uintptr_t> m; // old to new
	for (auto nit=given_nodes.begin();nit!=given_nodes.end();++nit) {
		auto p = nodes.emplace((*nit)->Copy());
		assert(p.second);
		m.emplace(reinterpret_cast<std::uintptr_t>(nit->get()),
				  reinterpret_cast<std::uintptr_t>(p.first->get()));
	}
	for (auto p : given_pm) {
		auto it1 = m.find(p.first);
		assert(it1 != m.end());
		if (p.second) {
			auto it2 = m.find(p.second);
			assert(it2 != m.end());
			pm.emplace(it1->second, it2->second);
		} else {
			pm.emplace(it1->second, 0);
		}
	}
}

}
}
