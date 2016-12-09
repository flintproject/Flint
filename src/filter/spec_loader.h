/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILTER_SPEC_LOADER_H_
#define FLINT_FILTER_SPEC_LOADER_H_

#include <cstring>
#include <memory>
#include <string>
#include <fstream>

#include <boost/uuid/string_generator.hpp>

namespace flint {

class SpecLoader {
public:
	SpecLoader(const SpecLoader &) = delete;
	SpecLoader &operator=(const SpecLoader &) = delete;

	explicit SpecLoader(const std::string &file) : ifs_(file.c_str(), std::ios::in) {}

	~SpecLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename TFilter>
	bool Load(TFilter *filter) {
		const size_t kLineSize = 1024; // FIXME
		const size_t kUuidSize = 36;

		if (!ifs_.is_open()) {
			std::cerr << "failed to open spec file" << std::endl;
			return false;
		}

		boost::uuids::string_generator gen;
		std::unique_ptr<char[]> line(new char[kLineSize]);
		boost::uuids::uuid u;
		while (ifs_.getline(line.get(), kLineSize)) {
			if (std::strlen(line.get()) < kUuidSize + 2) {
				std::cerr << "invalid line: " << line.get() << std::endl;
				return false;
			}
			line[kUuidSize] = '\0';
			u = gen(line.get());
			filter->AddSpec(u, line.get()+kUuidSize+1);
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

}

#endif
