/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>

#include <boost/program_options.hpp>

#include "bc/binary.h"

#include "cli.pb.h"

namespace po = boost::program_options;

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	std::string model_file, output_file;
	int print_help = 0;

	opts.add_options()
		("model", po::value<std::string>(&model_file), "Model file name")
		("output", po::value<std::string>(&output_file), "Output file name")
		("help,h", "Show this message");
	popts.add("model", 1).add("output", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) print_help = 1;
		if ( vm.count("model") == 0 ||
			 vm.count("output") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		std::cerr << "usage: " << argv[0] << " MODEL OUTPUT" << std::endl;
		std::cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	RequestBinaryStdio();

	std::unique_ptr<cli::RunOption> option(new cli::RunOption);
	option->set_model_filename(model_file);
	option->set_output_filename(output_file);

	int bs = option->ByteSize();
	std::unique_ptr<char[]> ba(new char[bs]);
	if (!option->SerializeToArray(ba.get(), bs)) {
		std::cerr << "failed to serialize option" << std::endl;
		return EXIT_FAILURE;
	}
	std::fwrite(ba.get(), bs, 1, stdout);

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
