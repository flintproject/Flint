/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

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

static const int kUuidSize = 36;

namespace {

typedef std::vector<pair<string, string> > InputVector;

class InputLoader : boost::noncopyable {
public:
	explicit InputLoader(const string &file) : ifs_(file.c_str(), std::ios::in) {}

	~InputLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	bool Load(InputVector *iv) {
		static const int kLineSize = kUuidSize + 256; // FIXME

		if (!ifs_.is_open()) {
			cerr << "failed to open input file" << endl;
			return false;
		}
		boost::scoped_array<char> line(new char[kLineSize]);
		while (ifs_.getline(line.get(), kLineSize)) {
			size_t len = strlen(line.get());
			if (len < kUuidSize+2) {
				cerr << "invalid line: " << line.get() << endl;
				return false;
			}
			iv->push_back(make_pair(string(line.get(), kUuidSize),
									string(line.get()+kUuidSize+1)));
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

bool TouchFile(const char *file)
{
	FILE *fp = std::fopen(file, "w");
	if (!fp) {
		cerr << "failed to touch " << file << endl;
		return false;
	}
	std::fclose(fp);
	return true;
}

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string input, db_file, name_file, value_file, function_file, ode_file, combine;
	int print_help = 0;

	opts.add_options()
		("input", po::value<string>(&input), "Input file")
		("db", po::value<string>(&db_file), "Input database file")
		("name", po::value<string>(&name_file), "Output name file")
		("value", po::value<string>(&value_file), "Output value file")
		("function", po::value<string>(&function_file), "Output function file")
		("ode", po::value<string>(&ode_file), "Output ode file")
		("combine", po::value<string>(&combine)->default_value("flint-combine"), "combine command")
		("help,h", "Show this message");
	popts.add("input", 1).add("db", 1).add("name", 1).add("value", 1).add("function", 1).add("ode", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("input") == 0 ||
					vm.count("db") == 0 ||
					vm.count("name") == 0 ||
					vm.count("value") == 0 ||
					vm.count("function") == 0 ||
					vm.count("ode") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " INPUT DB NAME VALUE FUNCTION ODE" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	boost::scoped_ptr<InputVector> iv(new InputVector);
	{
		boost::scoped_ptr<InputLoader> loader(new InputLoader(input));
		if (!loader->Load(iv.get())) {
			return EXIT_FAILURE;
		}
	}

	if (!TouchFile(name_file.c_str())) return EXIT_FAILURE;
	if (!TouchFile(value_file.c_str())) return EXIT_FAILURE;
	if (!TouchFile(function_file.c_str())) return EXIT_FAILURE;
	if (!TouchFile(ode_file.c_str())) return EXIT_FAILURE;

	for (InputVector::const_iterator it=iv->begin();it!=iv->end();++it) {
		const string &uuid = it->first;
		const string &path = it->second;
		std::ostringstream oss;
		oss << combine.c_str()
			<< ' '
			<< uuid.c_str()
			<< ' '
			<< db_file
			<< ' '
			<< name_file
			<< ' '
			<< value_file
			<< ' '
			<< function_file
			<< ' '
			<< ode_file
			<< " < "
			<< path;
		int r = RunSystem(oss.str().c_str());
		if (r != 0) return r;
	}

	return EXIT_SUCCESS;
}
