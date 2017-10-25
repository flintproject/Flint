/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "file.h"

#include <cstdio>
#include "utf8path.h"

namespace flint {
namespace file {

bool Txt(const char *filename, Format *format, const boost::filesystem::path &dir)
{
	boost::filesystem::path path = GetPathFromUtf8(filename);
	if (path.empty())
		return false;
	std::string path_s = path.string();
	if (!DetectFormat(path_s.c_str(), format))
		return false;
	FILE *fp;
	if (dir.empty()) {
		fp = std::fopen("file.txt", "wb");
	} else {
		auto file_txt = dir / "file.txt";
		fp = std::fopen(file_txt.string().c_str(), "wb");
	}
	if (!fp) {
		std::perror(filename);
		return false;
	}
	switch (*format) {
	case file::kCellml:
		fprintf(fp, "cellml\n");
		break;
	case file::kIsml:
		fprintf(fp, "isml\n");
		break;
	case file::kMathml:
		fprintf(fp, "mathml\n");
		break;
	case file::kPhml:
		fprintf(fp, "phml\n");
		break;
	case file::kPhsp:
		fprintf(fp, "phsp\n");
		break;
	case file::kPhz:
		fprintf(fp, "phz\n");
		break;
	case file::kSbml:
		fprintf(fp, "sbml\n");
		break;
	case file::kSedml:
		fprintf(fp, "sedml\n");
		break;
	}
	std::fclose(fp);
	return true;
}

}
}
