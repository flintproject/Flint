/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILTER_SPEC_LOADER_H_
#define FLINT_FILTER_SPEC_LOADER_H_

#include <cstring>
#include <memory>
#include <string>
#include <fstream>

#include <boost/scoped_array.hpp>
#include <boost/uuid/string_generator.hpp>

class SpecLoader : boost::noncopyable {
public:
	explicit SpecLoader(const std::string &file) : ifs_(file.c_str(), std::ios::in) {}

	~SpecLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename TFilter>
	bool Load(TFilter *filter) {
		using std::cerr;
		using std::endl;
		using std::strlen;

		static const size_t kLineSize = 1024; // FIXME
		static const size_t kUuidSize = 36;

		if (!ifs_.is_open()) {
			cerr << "failed to open spec file" << endl;
			return false;
		}

		boost::uuids::string_generator gen;
		boost::scoped_array<char> line(new char[kLineSize]);
		boost::uuids::uuid u;
		while (ifs_.getline(line.get(), kLineSize)) {
			if (strlen(line.get()) < kUuidSize + 2) {
				cerr << "invalid line: " << line.get() << endl;
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

#endif
