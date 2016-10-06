/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <memory>
#include <map>
#include <string>
#include <vector>

#include "isdf/reader.h"

using std::fclose;
using std::fopen;
using std::fwrite;
using std::perror;
using std::strcmp;
using std::string;

using namespace flint;

namespace {

class ColumnHandler {
public:
	explicit ColumnHandler(std::uint32_t skip, FILE *fp)
		: skip_(skip),
		  fp_(fp),
		  leading_descs_(),
		  rest_descs_(),
		  units_()
	{}

	void GetDescription(std::uint32_t i, std::uint32_t len, const char *d)
	{
		string desc(d, len);
		if (i < skip_) {
			leading_descs_.push_back(desc);
		} else {
			rest_descs_.emplace(desc, i);
		}
	}

	void GetUnit(std::uint32_t /*i*/, std::uint32_t len, const char *u)
	{
		units_.push_back(string(u, len));
	}

	int GetStep(size_t size, const char *buf)
	{
		if (skip_ > 0) {
			fwrite(buf, sizeof(double), skip_, fp_);
		}
		for (std::map<string, std::uint32_t>::const_iterator it=rest_descs_.begin();it!=rest_descs_.end();++it) {
			size_t i = it->second * sizeof(double);
			assert(i < size);
			fwrite(buf + i, sizeof(double), 1, fp_);
		}
		return 1;
	}

	void WriteDescriptionsAndUnits()
	{
		for (const auto &desc : leading_descs_)
			WriteEntry(desc);
		for (std::map<string, std::uint32_t>::const_iterator it=rest_descs_.begin();it!=rest_descs_.end();++it) {
			WriteEntry(it->first);
		}
		if (!units_.empty()) {
			for (std::uint32_t i=0;i<skip_;i++) {
				WriteEntry(units_[i]);
			}
			for (std::map<string, std::uint32_t>::const_iterator it=rest_descs_.begin();it!=rest_descs_.end();++it) {
				WriteEntry(units_[it->second]);
			}
		}
	}

private:
	void WriteEntry(const string &d)
	{
		std::uint32_t len = static_cast<std::uint32_t>(d.size());
		fwrite(&len, sizeof(len), 1, fp_);
		if (len > 0) fwrite(d.c_str(), len, 1, fp_);
	}

	std::uint32_t skip_;
	FILE *fp_;
	std::vector<string> leading_descs_;
	std::map<string, std::uint32_t> rest_descs_;
	std::vector<string> units_;
};

bool CopyFile(const char *source, const char *target)
{
	static const size_t kLength = 4096;

	FILE *ifp = fopen(source, "rb");
	if (!ifp) {
		perror(source);
		return false;
	}
	FILE *ofp = fopen(target, "wb");
	if (!ofp) {
		perror(target);
		fclose(ifp);
		return false;
	}
	std::unique_ptr<char[]> buf(new char[kLength]);
	size_t s;
	do {
		if ( (s = fread(buf.get(), 1, kLength, ifp)) == 0) {
			fclose(ofp);
			if (feof(ifp)) {
				fclose(ifp);
				return true;
			}
			fclose(ifp);
			std::cerr << "failed to read file: " << source << std::endl;
			return false;
		}
	} while (fwrite(buf.get(), s, 1, ofp) == 1);
	fclose(ofp);
	fclose(ifp);
	std::cerr << "failed to write file: " << target << std::endl;
	return false;
}

class Writer {
public:
	Writer(const Writer &) = delete;
	Writer &operator=(const Writer &) = delete;

	explicit Writer(FILE *fp)
		: fp_(fp)
	{
		assert(fp);
	}

	~Writer() {
		fclose(fp_);
	}

	bool Write(std::uint32_t skip, isdf::Reader *reader, std::istream *is) {
		fwrite(&reader->header(), sizeof(isdf::ISDFHeader), 1, fp_);

		if (!reader->ReadComment(is)) return false;
		fwrite(reader->comment(), reader->num_bytes_comment(), 1, fp_);

		std::unique_ptr<ColumnHandler> handler(new ColumnHandler(skip, fp_));
		if ( !reader->ReadDescriptions(*handler, is) ||
			 !reader->ReadUnits(*handler, is) ) return false;
		handler->WriteDescriptionsAndUnits();
		return reader->ReadSteps(*handler, is);
	}

private:
	FILE *fp_;
};

void Usage()
{
	std::cerr << "usage: isdsort INPUT OUTPUT [SKIP]" << std::endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc < 2 || 4 < argc) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
		Usage();
		return EXIT_SUCCESS;
	}
	if (argc == 2) {
		Usage();
		return EXIT_FAILURE;
	}

	assert(argc == 3 || argc == 4);
	std::uint32_t skip = 0;
	if (argc == 4) {
		int s = std::atoi(argv[3]);
		if (s <= 0) {
			std::cerr << "invalid skip: " << argv[3] << std::endl;
			return EXIT_FAILURE;
		}
		skip = static_cast<std::uint32_t>(s);
	}

	std::ifstream ifs(argv[1], std::ios::in|std::ios::binary);
	if (!ifs.is_open()) {
		std::cerr << "failed to open file: " << argv[1] << std::endl;
		return EXIT_FAILURE;
	}
	std::unique_ptr<isdf::Reader> reader(new isdf::Reader);
	if (!reader->ReadHeader(&ifs)) return EXIT_FAILURE;

	if (reader->num_objs() <= skip) {
		ifs.close();
		// no need to change order of columns
		return (CopyFile(argv[1], argv[2])) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	FILE *fp = fopen(argv[2], "wb");
	if (!fp) {
		perror(argv[2]);
		return EXIT_FAILURE;
	}
	{
		Writer writer(fp);
		if (!writer.Write(skip, reader.get(), &ifs))
			return EXIT_FAILURE;
	}
	ifs.close();

	return EXIT_SUCCESS;
}
