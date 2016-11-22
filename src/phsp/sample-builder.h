/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHSP_SAMPLE_BUILDER_H_
#define FLINT_PHSP_SAMPLE_BUILDER_H_

#include "phsp/parameter-element.h"
#include "phsp/product-element.h"
#include "phsp/zip-element.h"

#include <memory>
#include <stack>
#include <string>
#include <utility>
#include <vector>

namespace flint {
namespace phsp {

class ParameterElement;
class ProductElement;
class ZipElement;

class SampleBuilder {
public:
	SampleBuilder();

	void PushParameter(const std::string &name, std::vector<double> &&values);
	void PushProduct();
	void PushZip();
	bool Pop();

	SampleElement *Build();

private:
	enum class SampleType {
		kParameter,
		kProduct,
		kZip
	};

	std::stack<std::unique_ptr<ParameterElement> > parameters_;
	std::stack<std::unique_ptr<ProductElement> > products_;
	std::stack<std::unique_ptr<ZipElement> > zips_;
	std::stack<std::pair<SampleType, bool> > stack_;
};

}
}

#endif
