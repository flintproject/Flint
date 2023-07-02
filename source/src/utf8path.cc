/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/utf8path.h"

#include <cassert>
#include <cstring>
#include <iostream>

#ifdef _WIN32
#include <cstdio>
#include <cstdlib>
#include <memory>

#include <windows.h>
#endif

namespace flint {

boost::filesystem::path GetPathFromUtf8(const char *utf8)
{
	if (!utf8)
		return {};
#ifdef _WIN32
	int len = MultiByteToWideChar(CP_UTF8,
								  MB_ERR_INVALID_CHARS,
								  utf8,
								  -1,
								  nullptr,
								  0);
	if (len == 0) {
		std::cerr << "failed to convert file name encoded in UTF-8: "
				  << utf8
				  << ": "
				  << __FILE__ ":" << __LINE__
				  << std::endl;
		return {};
	}
	std::unique_ptr<wchar_t[]> buf(new wchar_t[len]);
	int r = MultiByteToWideChar(CP_UTF8,
								MB_ERR_INVALID_CHARS,
								utf8,
								-1,
								buf.get(),
								len);
	if (r == 0) {
		std::cerr << "failed to convert file name encoded in UTF-8: "
				  << utf8
				  << ": "
				  << __FILE__ ":" << __LINE__
				  << std::endl;
		return {};
	}
	boost::filesystem::path path(buf.get());
	return path;
#else
	boost::filesystem::path path(utf8);
	return path;
#endif
}

char *GetUtf8FromPath(const boost::filesystem::path &path)
{
	if (path.empty())
		return nullptr;
#ifdef _WIN32
	const std::wstring &path_ws(path.wstring());
	int len = WideCharToMultiByte(CP_UTF8,
								  0, // TODO: WC_ERR_INVALID_CHARS
								  path_ws.c_str(),
								  path_ws.size(),
								  nullptr,
								  0,
								  nullptr,
								  nullptr);
	if (len == 0) {
		std::cerr << "failed to convert path into a file name encoded in UTF-8: "
				  << path
				  << ": "
				  << __FILE__ ":" << __LINE__
				  << std::endl;
		return nullptr;
	}
	char *utf8 = new char[len+1];
	int r = WideCharToMultiByte(CP_UTF8,
								0, // TODO: WC_ERR_INVALID_CHARS
								path_ws.c_str(),
								path_ws.size(),
								utf8,
								len,
								nullptr,
								nullptr);
	if (r == 0) {
		std::cerr << "failed to convert path into file name encoded in UTF-8: "
				  << path
				  << ": "
				  << __FILE__ ":" << __LINE__
				  << std::endl;
		return nullptr;
	}
	utf8[len] = '\0';
	return utf8;
#else
	const std::string &path_s(path.string());
	size_t s = path_s.size();
	assert(s > 0);
	char *utf8 = new char[s+1];
	std::memcpy(utf8, path_s.c_str(), s);
	utf8[s] = '\0';
	return utf8;
#endif
}

}
