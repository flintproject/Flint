/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHSP_SAMPLE_ELEMENT_H_
#define FLINT_PHSP_SAMPLE_ELEMENT_H_

#include <string>
#include <vector>

namespace flint {
namespace phsp {

class SampleElement {
public:
	virtual ~SampleElement() = default;

	virtual size_t GetCount() const = 0;

	virtual void GetColumns(std::vector<std::string> *names) const = 0;

	virtual size_t GetNumOfCols() const = 0;

	virtual void Fill(size_t k, double *tuple) const = 0;
};

}
}

#endif
