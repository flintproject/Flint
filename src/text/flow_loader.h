/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TEXT_FLOW_LOADER_H_
#define FLINT_TEXT_FLOW_LOADER_H_

#include <cstdio>
#include <fstream>
#include <iostream>
#include <string>
#include <utility>

#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>

namespace text {

class FlowLoader : boost::noncopyable {
public:
	explicit FlowLoader(const std::string &file) : ifs_(file.c_str(), std::ios::in) {}

	~FlowLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		using std::cerr;
		using std::endl;

		static const int kLineSize = 32; // FIXME

		if (!ifs_.is_open()) {
			cerr << "failed to open flow file" << endl;
			return false;
		}

		boost::scoped_array<char> line(new char[kLineSize]);
		while (ifs_.getline(line.get(), kLineSize)) {
			int n0, n1;
			int r = std::sscanf(line.get(), "%d%d", &n0, &n1);
			if (r == EOF || r < 2) {
				cerr << "invalid flow: " << line.get() << endl;
				return false;
			}
			if (n0 <= 0) {
				cerr << "invalid source of flow: " << line.get() << endl;
				return false;
			}
			if (n1 <= 0) {
				cerr << "invalid target of flow: " << line.get() << endl;
				return false;
			}
			if (!handler->Handle(n0, n1)) return false;
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

} // namespace text

#endif
