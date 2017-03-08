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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wfstream.h>
#include <wx/zipstrm.h>
#pragma GCC diagnostic pop

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
	wxFileInputStream fis(filename.get()); // TODO: locale-dependency?
	if (!fis) {
		std::cerr << "failed to open " << filename.get() << std::endl;
		return false;
	}
	wxZipInputStream zis(fis);

	boost::filesystem::path tp(dir);

	std::unique_ptr<wxZipEntry> entry;
	while (entry.reset(zis.GetNextEntry()), entry) {
		if (entry->IsDir()) {
			// skip a directory entry
			continue;
		}
		wxString name = entry->GetName();
		if (name.empty()) {
			std::cerr << "found empty name in the zip archive" << std::endl;
			continue;
		}
		if (!zis.OpenEntry(*entry)) {
			std::cerr << "failed to open a zip entry: " << name << std::endl;
			return false;
		}
		auto name_s = name.ToStdString();
		if (name_s.empty()) {
			std::cerr << "data loss in converting to std::string: "
					  << name
					  << std::endl;
			continue;
		}

		boost::filesystem::path op(tp);
		op /= name_s;
		boost::filesystem::path pp(op.parent_path());
		boost::filesystem::create_directories(pp);

		std::string op_s = op.string();
		FILE *ofp = std::fopen(op_s.c_str(), "wb");
		if (!ofp) {
			std::perror(op_s.c_str());
			return false;
		}

		size_t size = static_cast<size_t>(entry->GetSize());
		std::unique_ptr<char[]> buf(new char[size]);
		if (!zis.Read(buf.get(), size)) {
			std::cerr << "failed to read zip entry of name " << name << std::endl;
			std::fclose(ofp);
			return false;
		}
		if (std::fwrite(buf.get(), size, 1, ofp) != 1) {
			std::cerr << "failed to write entry of name "
					  << name
					  << " in the zip archive: "
					  << filename.get()
					  << std::endl;
			std::fclose(ofp);
			return false;
		}
		std::fclose(ofp);
	}

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
