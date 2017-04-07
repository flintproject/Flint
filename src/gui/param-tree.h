/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_PARAM_TREE_H_
#define FLINT_GUI_PARAM_TREE_H_

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <unordered_set>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class ParamTreeNode {
public:
	virtual ~ParamTreeNode();

	virtual ParamTreeNode *Copy() const = 0;

	virtual wxString GetType() const = 0;
	virtual wxString GetName() const = 0;
	virtual wxString GetSummary() const = 0;
};

struct ParamTree {
	~ParamTree();

	void Import(const std::unordered_set<std::unique_ptr<ParamTreeNode> > &given_nodes,
				const std::unordered_map<std::uintptr_t, std::uintptr_t> &given_pm);

	std::unordered_set<std::unique_ptr<ParamTreeNode> > nodes;
	std::unordered_map<std::uintptr_t, std::uintptr_t> pm; // parent map: id to parent id
};

}
}

#endif
