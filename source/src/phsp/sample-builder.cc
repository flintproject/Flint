/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "phsp/sample-builder.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <iostream>

namespace flint {
namespace phsp {

SampleBuilder::SampleBuilder()
	: parameters_()
	, products_()
	, zips_()
	, stack_()
{
	stack_.emplace(SampleType::kProduct, false);
}

void SampleBuilder::PushParameter(const std::string &name, std::vector<double> &&values)
{
	parameters_.emplace(new ParameterElement(name, std::move(values)));
	stack_.emplace(SampleType::kParameter, true);
}

void SampleBuilder::PushProduct()
{
	stack_.emplace(SampleType::kProduct, false);
}

void SampleBuilder::PushZip()
{
	stack_.emplace(SampleType::kZip, false);
}

bool SampleBuilder::Pop()
{
	std::vector<std::unique_ptr<SampleElement> > children;

	while (!stack_.empty()) {
		auto top = std::move(stack_.top());
		stack_.pop();

		if (top.second) {
			switch (top.first) {
			case SampleType::kParameter:
				children.push_back(std::move(parameters_.top()));
				parameters_.pop();
				break;
			case SampleType::kProduct:
				children.push_back(std::move(products_.top()));
				products_.pop();
				break;
			case SampleType::kZip:
				children.push_back(std::move(zips_.top()));
				zips_.pop();
				break;
			}
			continue;
		}

		std::reverse(children.begin(), children.end());
		switch (top.first) {
		case SampleType::kParameter:
			assert(false);
			return false;
		case SampleType::kProduct:
			products_.emplace(new ProductElement(std::move(children)));
			stack_.emplace(SampleType::kProduct, true);
			return true;
		case SampleType::kZip:
			zips_.emplace(new ZipElement(std::move(children)));
			stack_.emplace(SampleType::kZip, true);
			return true;
		}
	}
	std::cerr << "stacks of <parameter-set> are broken: "
			  << children.size() << std::endl;
	return false;
}

SampleElement *SampleBuilder::Build()
{
	Pop();
	if (!parameters_.empty()) {
		std::cerr << "stack of <parameter> is non-empty: "
				  << parameters_.size() << std::endl;
		return nullptr;
	}
	if (products_.size() != 1) {
		std::cerr << "stack of products is broken: "
				  << products_.size() << std::endl;
		return nullptr;
	}
	if (!zips_.empty()) {
		std::cerr << "stack of <zip>s is non-empty: "
				  << zips_.size() << std::endl;
		return nullptr;
	}
	std::unique_ptr<ProductElement> product = std::move(products_.top());
	products_.pop();

	std::vector<std::string> names;
	product->GetColumns(&names);
	auto n = names.size();
	assert(n == product->GetNumOfCols());
	std::unique_ptr<double[]> tuple(new double[n]);
	auto count = product->GetCount();
	for (size_t k=0;k<count;k++)
		product->Fill(k, tuple.get());
	return product.release();
}

}
}
