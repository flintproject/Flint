/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "modelpath.h"

#include <cstdio>
#include <cstring>
#include <memory>
#include <string>

#include "database.h"
#include "utf8path.h"

namespace flint {

namespace {

typedef int (*FindFunction)(sqlite3 *, char *);

char *GetInputFilename(sqlite3 *db, FindFunction f)
{
	std::unique_ptr<char[]> utf8(new char[1024]); // FIXME
	if (!f(db, utf8.get()))
		return nullptr;
	boost::filesystem::path path = GetPathFromUtf8(utf8.get());
	if (path.empty())
		return nullptr;
	std::string path_s = path.string();
	size_t s = path_s.size();
	char *filename = new char[s+1]; // FIXME
	if (s > 0)
		std::memcpy(filename, path_s.c_str(), s);
	filename[s] = '\0';
	return filename;
}

} // namespace

char *GetGivenFilename(sqlite3 *db)
{
	return GetInputFilename(db, FindGivenFile);
}

char *GetModelFilename(sqlite3 *db)
{
	return GetInputFilename(db, FindModelFile);
}

}