/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include "system.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::pair;
using std::printf;
using std::sprintf;
using std::string;
using std::strlen;
using std::make_pair;

static const size_t kUuidSize = 36;

namespace {

typedef std::vector<string> DumpVector;

class DumpLoader : boost::noncopyable {
public:
	explicit DumpLoader(const string &file) : ifs_(file.c_str(), std::ios::in) {}

	~DumpLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	bool Load(DumpVector *iv) {
		static const int kLineSize = kUuidSize + 2; // FIXME

		if (!ifs_.is_open()) {
			cerr << "failed to open dump file" << endl;
			return false;
		}
		boost::scoped_array<char> line(new char[kLineSize]);
		while (ifs_.getline(line.get(), kLineSize)) {
			size_t len = strlen(line.get());
			if (len != kUuidSize) {
				cerr << "invalid line: " << line.get() << endl;
				return false;
			}
			iv->push_back(line.get());
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string input, sbml;
	int print_help = 0;

	opts.add_options()
		("input", po::value<string>(&input), "Input file")
		("sbml", po::value<string>(&sbml)->default_value("flint-sbml"), "sbml command")
		("help,h", "Show this message");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("input") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " INPUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	boost::scoped_ptr<DumpVector> iv(new DumpVector);
	{
		boost::scoped_ptr<DumpLoader> loader(new DumpLoader(input));
		if (!loader->Load(iv.get())) {
			return EXIT_FAILURE;
		}
	}
	boost::system::error_code ec;
	boost::filesystem::path temp_path = boost::filesystem::temp_directory_path(ec);
	if (ec) {
		cerr << ec.message() << endl;
		return EXIT_FAILURE;
	}
	temp_path /= "%%%%-%%%%-%%%%-%%%%.txt";
	for (DumpVector::const_iterator it=iv->begin();it!=iv->end();++it) {
		boost::filesystem::path output_path = boost::filesystem::unique_path(temp_path, ec);
		if (ec) {
			cerr << ec << endl;
			return EXIT_FAILURE;
		}
		if (boost::filesystem::exists(output_path, ec)) {
			cerr << "failed to create temporary path: " << output_path << endl;
			return EXIT_FAILURE;
		}
		std::string opath = output_path.string();
		size_t len0 = strlen(sbml.c_str());
		size_t len1 = strlen(it->c_str());
		size_t len2 = opath.size();
		boost::scoped_array<char> buf(new char[len0 + len1 + len2 + 32]); // FIXME
		sprintf(buf.get(), "%s %s.db > %s", sbml.c_str(), it->c_str(), opath.c_str());
		int r = RunSystem(buf.get());
		if (r != 0) return r;
		printf("%s %s\n", it->c_str(), opath.c_str());
	}

	return EXIT_SUCCESS;
}
