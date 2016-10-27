/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phz.h"

#include <cassert>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include <zip.h>

#include "database.h"
#include "modelpath.h"
#include "utf8path.h"

namespace flint {
namespace phz {

bool Read(sqlite3 *db, const char *dir)
{
	std::unique_ptr<char[]> filename(GetGivenFilename(db));
	if (!filename)
		return false;
	int ze;
	struct zip *zp = zip_open(filename.get(), 0, &ze);
	if (!zp) {
		char buf[1024];
		int len = zip_error_to_str(buf, 1023, ze, errno);
		assert(len < 1024);
		buf[len] = '\0';
		std::cerr << buf << std::endl;
		return false;
	}
	zip_int64_t ne = zip_get_num_entries(zp, 0);
	if (ne < 0) {
		std::cerr << "the zip archive is null: "
			 << filename.get()
			 << std::endl;
		return false;
	}
	if (ne == 0) {
		std::cerr << "the zip archive is empty: "
			 << filename.get()
			 << std::endl;
		return false;
	}

	boost::filesystem::path tp(dir);

	for (zip_int64_t i=0;i<ne;i++) {
		const char *name = zip_get_name(zp, i, 0);
		if (!name) {
			std::cerr << zip_strerror(zp) << std::endl;
			return false;
		}
		size_t nlen = strlen(name);
		if (nlen <= 1) {
			std::cerr << "unexpected entry in the zip archive: "
				 << name
				 << std::endl;
			continue;
		} else if (name[nlen-1] == '/') {
			// skip a directory entry
			continue;
		}

		boost::filesystem::path op(tp);
		op /= name;
		boost::filesystem::path pp(op.parent_path());
		boost::filesystem::create_directories(pp);

		std::string op_s = op.string();
		FILE *ofp = fopen(op_s.c_str(), "wb");
		if (!ofp) {
			std::perror(op_s.c_str());
			return false;
		}

		struct zip_file *zfp = zip_fopen_index(zp, i, 0);
		if (!zfp) {
			std::cerr << zip_strerror(zp) << std::endl;
			fclose(ofp);
			return false;
		}

		char buf[1024];
		int len;
		while ( (len = zip_fread(zfp, buf, 1024)) ) {
			if (len < 0) {
				std::cerr << "failed to read entry of index "
					 << i
					 << " in the zip archive: "
					 << filename.get()
					 << std::endl;
				fclose(ofp);
				return false;
			}
			assert(len <= 1024);
			if (fwrite(buf, len, 1, ofp) != 1) {
				std::cerr << "failed to write entry of index "
					 << i
					 << " in the zip archive: "
					 << filename.get()
					 << std::endl;
				fclose(ofp);
				return false;
			}
		}
		zip_fclose(zfp);

		fclose(ofp);
	}
	zip_close(zp);

	tp /= "model.phml";
	if (!boost::filesystem::is_regular_file(tp)) {
		std::cerr << "missing model.phml in the zip archive: "
			 << filename.get()
			 << std::endl;
		return false;
	}

	boost::filesystem::path amp = boost::filesystem::absolute(tp);
	std::unique_ptr<char[]> mf(GetUtf8FromPath(amp));
	if (!mf)
		return false;
	if (strlen(mf.get()) >= 1024) {
		std::cerr << "resulting filename is too long: "
			 << mf.get()
			 << std::endl;
		return false;
	}
	if (!SaveModelFile(db, mf.get())) return false;

	return true;
}

}
}
