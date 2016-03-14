/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef SYS_TEMPORARY_PATH_H_
#define SYS_TEMPORARY_PATH_H_

#include <string>

namespace flint {

class TemporaryPath {
public:
	explicit TemporaryPath(const std::string &name)
		: name_(name)
	{}

	~TemporaryPath() {}

	char *Touch();

private:
	std::string name_;
};

}

#endif // SYS_TEMPORARY_PATH_H_
