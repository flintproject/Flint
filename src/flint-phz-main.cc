/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include <zip.h>

#include "database.h"
#include "modelpath.h"
#include "utf8path.h"

using std::cerr;
using std::endl;
using std::perror;
using std::strcmp;
using std::string;
using std::strncmp;

namespace {

void Usage()
{
	cerr << "flint-phz DB TARGETDIR" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc == 2) {
		Usage();
		if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}
	if (argc != 3) {
		Usage();
		return EXIT_FAILURE;
	}

	boost::scoped_array<char> filename(GetGivenFilename(argv[1]));
	int ze;
	struct zip *zp = zip_open(filename.get(), 0, &ze);
	if (!zp) {
		char buf[1024];
		int len = zip_error_to_str(buf, 1023, ze, errno);
		assert(len < 1024);
		buf[len] = '\0';
		cerr << buf << endl;
		return EXIT_FAILURE;
	}
	zip_int64_t ne = zip_get_num_entries(zp, 0);
	if (ne < 0) {
		cerr << "the zip archive is null: "
			 << filename.get()
			 << endl;
		return EXIT_FAILURE;
	}
	if (ne == 0) {
		cerr << "the zip archive is empty: "
			 << filename.get()
			 << endl;
		return EXIT_FAILURE;
	}

	boost::filesystem::path tp(argv[2]);

	for (zip_int64_t i=0;i<ne;i++) {
		const char *name = zip_get_name(zp, i, 0);
		if (!name) {
			cerr << zip_strerror(zp) << endl;
			return EXIT_FAILURE;
		}
		size_t nlen = strlen(name);
		if (nlen <= 1) {
			cerr << "unexpected entry in the zip archive: "
				 << name
				 << endl;
			continue;
		} else if (name[nlen-1] == '/') {
			// skip a directory entry
			continue;
		}

		boost::filesystem::path op(tp);
		op /= name;
		boost::filesystem::path pp(op.parent_path());
		boost::filesystem::create_directories(pp);

		string op_s = op.string();
		FILE *ofp = fopen(op_s.c_str(), "wb");
		if (!ofp) {
			perror(op_s.c_str());
			return EXIT_FAILURE;
		}

		struct zip_file *zfp = zip_fopen_index(zp, i, 0);
		if (!zfp) {
			cerr << zip_strerror(zp) << endl;
			return EXIT_FAILURE;
		}

		char buf[1024];
		int len;
		while ( (len = zip_fread(zfp, buf, 1024)) ) {
			if (len < 0) {
				cerr << "failed to read entry of index "
					 << i
					 << " in the zip archive: "
					 << filename.get()
					 << endl;
				return EXIT_FAILURE;
			}
			assert(len <= 1024);
			if (fwrite(buf, len, 1, ofp) != 1) {
				cerr << "failed to write entry of index "
					 << i
					 << " in the zip archive: "
					 << filename.get()
					 << endl;
				return EXIT_FAILURE;
			}
		}
		zip_fclose(zfp);

		fclose(ofp);
	}
	zip_close(zp);

	tp /= "model.phml";
	if (!boost::filesystem::is_regular_file(tp)) {
		cerr << "missing model.phml in the zip archive: "
			 << filename.get()
			 << endl;
		return EXIT_FAILURE;
	}

	boost::filesystem::path amp = boost::filesystem::absolute(tp);
	boost::scoped_array<char> mf(GetUtf8FromPath(amp));
	if (strlen(mf.get()) >= 1024) {
		cerr << "resulting filename is too long: "
			 << mf.get()
			 << endl;
		return EXIT_FAILURE;
	}
	if (!SaveModelFile(argv[1], mf.get())) return EXIT_FAILURE;

	return EXIT_SUCCESS;
}
