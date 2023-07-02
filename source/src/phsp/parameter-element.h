/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHSP_PARAMETER_ELEMENT_H_
#define FLINT_PHSP_PARAMETER_ELEMENT_H_

#include "phsp/sample-element.h"

#include <cassert>
#include <string>
#include <vector>

namespace flint {
namespace phsp {

class ParameterElement : public SampleElement {
public:
	ParameterElement(const std::string &name, std::vector<double> &&values)
		: name_(name)
		, values_(std::move(values))
	{}

	virtual ~ParameterElement() = default;

	virtual size_t GetCount() const {
		return values_.size();
	}

	virtual void GetColumns(std::vector<std::string> *names) const {
		names->push_back(name_);
	}

	virtual size_t GetNumOfCols() const {
		return 1;
	}

	virtual void Fill(size_t k, double *tuple) const {
		assert(k < values_.size());
		*tuple = values_.at(k);
	}

private:
	std::string name_;
	std::vector<double> values_;
};

}
}

#endif
