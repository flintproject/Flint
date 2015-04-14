/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "modelpath.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>

#include <boost/scoped_array.hpp>

#include "database.h"
#include "utf8path.h"

namespace {

typedef int (*FindFunction)(const char *, char *);

char *GetInputFilename(const char *input, FindFunction f)
{
	boost::scoped_array<char> utf8(new char[1024]); // FIXME
	if (!f(input, utf8.get())) {
		std::exit(EXIT_FAILURE);
	}
	boost::filesystem::path path = GetPathFromUtf8(utf8.get());
	std::string path_s = path.string();
	size_t s = path_s.size();
	char *filename = new char[s+1]; // FIXME
	memcpy(filename, path_s.c_str(), s);
	filename[s] = '\0';
	return filename;
}

} // namespace

char *GetGivenFilename(const char *input)
{
	return GetInputFilename(input, FindGivenFile);
}

char *GetModelFilename(const char *input)
{
	return GetInputFilename(input, FindModelFile);
}
