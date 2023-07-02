/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_ERROR_H_
#define FLINT_ERROR_H_

#include <cstdio>
#include <iostream>
#include <sstream>
#include <string>

namespace flint {

class StderrCapture {
public:
	StderrCapture()
		: oss_()
		, orig_(std::cerr.rdbuf(oss_.rdbuf()))
	{}

	~StderrCapture() {
		std::cerr.rdbuf(orig_);
	}

	std::string Get() {
		return oss_.str();
	}

private:
	std::ostringstream oss_;
	std::streambuf *orig_;
};

class StderrRedirect {
public:
	explicit StderrRedirect(std::ostream *os)
		: orig_(std::cerr.rdbuf(os->rdbuf()))
	{}

	~StderrRedirect() {
		std::cerr.rdbuf(orig_);
	}

private:
	std::streambuf *orig_;
};

}

#endif
