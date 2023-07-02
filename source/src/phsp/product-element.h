/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHSP_PRODUCT_ELEMENT_H_
#define FLINT_PHSP_PRODUCT_ELEMENT_H_

#include "phsp/sample-element.h"

#include <memory>
#include <vector>

namespace flint {
namespace phsp {

class ProductElement : public SampleElement {
public:
	ProductElement(std::vector<std::unique_ptr<SampleElement> > &&children)
		: children_(std::move(children))
	{}

	virtual ~ProductElement() = default;

	virtual size_t GetCount() const {
		size_t s = 1;
		for (const auto &child : children_)
			s *= child->GetCount();
		return s;
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
			auto c = child->GetCount();
			auto j = k % c;
			k /= c;
			child->Fill(j, tuple);
			tuple += child->GetNumOfCols();
		}
	}

private:
	std::vector<std::unique_ptr<SampleElement> > children_;
};

}
}

#endif
