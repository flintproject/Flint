/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHSP_ZIP_ELEMENT_H_
#define FLINT_PHSP_ZIP_ELEMENT_H_

#include "phsp/sample-element.h"

#include <cassert>
#include <memory>
#include <vector>

namespace flint {
namespace phsp {

class ZipElement : public SampleElement {
public:
	ZipElement(std::vector<std::unique_ptr<SampleElement> > &&children)
		: children_(std::move(children))
	{}

	virtual ~ZipElement() = default;

	virtual size_t GetCount() const {
		auto n = children_.size();
		assert(n > 0);
		auto count = children_.at(0)->GetCount();
		for (size_t i=1;i<n;i++) {
			auto c = children_.at(i)->GetCount();
			if (c < count)
				count = c;
		}
		return count;
	}

	virtual void GetColumns(std::vector<std::string> *names) const {
		for (auto &child : children_)
			child->GetColumns(names);
	}

	virtual size_t GetNumOfCols() const {
		size_t n = 0;
		for (const auto &child : children_)
			n += child->GetNumOfCols();
		return n;
	}

	virtual void Fill(size_t k, double *tuple) const {
		for (const auto &child : children_) {
			child->Fill(k, tuple);
			tuple += child->GetNumOfCols();
		}
	}

private:
	std::vector<std::unique_ptr<SampleElement> > children_;
};

}
}

#endif
