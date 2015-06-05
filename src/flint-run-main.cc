/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/scoped_ptr.hpp>

#include "bc/binary.h"
#include "file.hh"
#include "load.hh"
#include "system.h"
#include "utf8path.h"
#include "workspace/task.h"

#include "cli.pb.h"

using std::cerr;
using std::endl;
using std::fprintf;
using std::strcmp;

namespace {

const size_t kInputLength = 8192;

void Usage()
{
	cerr << "usage: flint-run" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	if (argc == 2) {
		Usage();
		if ( strcmp(argv[1], "-h") == 0 ||
			 strcmp(argv[1], "--help") == 0 ) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}

	RequestBinaryStdio();
	// read input parameters from stdin
	char buffer[kInputLength];
	size_t s = std::fread(buffer, 1, kInputLength, stdin);
	if (s == 0) {
		cerr << "failed to read the input" << endl;
		return EXIT_FAILURE;
	}
	cli::RunOption option;
	if (!option.ParseFromArray(buffer, (int)s)) {
		cerr << "failed to parse the input" << endl;
		return EXIT_FAILURE;
	}

	boost::filesystem::path target_path = GetPathFromUtf8(option.output_filename().c_str());

	boost::scoped_ptr<workspace::Task> task(new workspace::Task(option.model_filename().c_str()));
	file::Format format;
	if (!task->Setup(&format)) {
		return EXIT_FAILURE;
	}

	if (option.has_spec_filename()) {
		boost::filesystem::path spec_path = GetPathFromUtf8(option.spec_filename().c_str());
		boost::system::error_code ec;
		if (!boost::filesystem::is_regular_file(spec_path, ec) || ec) {
			cerr << ec.message() << endl;
			return EXIT_FAILURE;
		}
		boost::filesystem::copy_file(spec_path, "spec.txt", ec);
		if (ec) {
			cerr << "failed to copy "
				 << option.spec_filename()
				 << " to spec.txt: "
				 << ec.message()
				 << endl;
			return EXIT_FAILURE;
		}
	}
	if (!load::Config(format, load::kRun))
		return EXIT_FAILURE;

	FILE *fp = fopen("run.mk", "w");
	if (!fp) {
		perror(argv[0]);
		return EXIT_FAILURE;
	}
	fprintf(fp, ".PHONY: all\n");
	fprintf(fp, "\n");
	fprintf(fp, "all: Makefile\n");
	fprintf(fp, "\t$(MAKE)\n");
	fprintf(fp, "\n");
	fprintf(fp, "Makefile: conf.txt\n");
	fprintf(fp, "\techo JOBS = 0 > $@\n");
	fprintf(fp, "\techo include load.mk >> $@\n");
	fprintf(fp, "\tflint-taskconfig run $< >> $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "conf.txt: load.mk\n");
	fprintf(fp, "\t$(MAKE) -f $< $@\n");
	if (option.has_granularity()) {
		fprintf(fp, "\techo %d >> $@\n", option.granularity());
	} else {
		fprintf(fp, "\techo 1 >> $@\n");
	}
	fprintf(fp, "\n");
	fclose(fp);

	int r = RunSystem("flint-make -j -rs -f run.mk");
	if (r != EXIT_SUCCESS) return r;

	// finally, copy output
	boost::filesystem::path output_path("0/isd");
	boost::system::error_code ec;
	bool b = boost::filesystem::exists(output_path, ec);
	if (ec) {
		cerr << "failed to stat file: "
			 << ec.message()
			 << endl;
		return EXIT_FAILURE;
	}
	if (!b) return EXIT_FAILURE;

	boost::filesystem::copy_file(output_path, target_path, ec);
	if (ec) {
		cerr << "failed to copy output to "
			 << option.output_filename()
			 << ": "
			 << ec.message()
			 << endl;
		return EXIT_FAILURE;
	}

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
