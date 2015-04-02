/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef SYS_TEMPORARY_PATH_H_
#define SYS_TEMPORARY_PATH_H_

#include <string>

class TemporaryPath {
public:
	TemporaryPath() : name_("tmp"), directory_() {}
	explicit TemporaryPath(const std::string &name)
		: name_(name), directory_() {}
	TemporaryPath(const std::string &name, const std::string &directory)
		: name_(name), directory_(directory) {}
	~TemporaryPath() {}

	char *Touch();
	char *Create(const void *data, size_t size);

private:
	std::string name_;
	std::string directory_;
};

#endif // SYS_TEMPORARY_PATH_H_
