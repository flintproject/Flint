/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include <boost/program_options.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/scoped_ptr.hpp>

#include "ipc.pb.h"
#include "unit.pb.h"
#include "bc/binary.h"
#include "bc/pack.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::printf;
using std::string;

using unit::Element;
using unit::Unit;

namespace {

typedef boost::ptr_map<int, Unit> UnitMap;

bool IsOfTime(const UnitMap &units, int id, long *denominator, long *numerator)
{
	UnitMap::const_iterator it = units.find(id);
	if (it == units.end()) {
		cerr << "missing unit with unit-id " << id << endl;
		return false;
	}
	const Unit *unit = it->second;
	if (unit->name() == "second") {
		*denominator = 1;
		*numerator = 1;
		return true;
	} else if (unit->element_size() == 1) {
		long d, n;
		const Element &element = unit->element(0);
		if (IsOfTime(units, element.unit_id(), &d, &n)) {
			if (element.has_exponent() && element.exponent() != 1) return false;
			if (element.has_offset() && element.offset() != 0) return false;
			if (element.has_factor()) {
				int factor = element.factor();
				if (factor > 0) {
					for (int i=0;i<factor;i++) {
						n *= 10;
					}
				} else if (factor < 0) {
					for (int i=0;i<-factor;i++) {
						d *= 10;
					}
				}
			}
			if (element.has_multiplier()) {
				double multiplier = element.multiplier();
				if (multiplier < 0) {
					cerr << "non-positional multiplier: " << multiplier << endl;
					return false;
				}
				long m = static_cast<long>(multiplier);
				if (static_cast<double>(m) != multiplier) {
					cerr << "multiplier was truncated: " << multiplier << endl;
				}
				n *= m;
			}

			*denominator = d;
			*numerator = n;
			return true;
		}
	}
	return false;
}

bool Load(std::istream *is, UnitMap *units)
{
	while (is->peek() != EOF) {
		std::auto_ptr<Unit> unit(new Unit);
		if (!UnpackFromIstream(*unit, is)) {
			cerr << "could not read Unit" << endl;
			return false;
		}
		int unit_id = unit->id();
		units->insert(unit_id, unit.release());
	}
	return true;
}

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	RequestBinaryStdio();

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string input_file;
	int print_help = 0;

	opts.add_options()
		("input", po::value<string>(&input_file), "Input file name")
		("text", "Specify plain text output")
		("help,h", "Show this message");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " [PATH]" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	UnitMap units;
	if (vm.count("input") == 0) {
		if (!Load(&std::cin, &units)) return EXIT_FAILURE;
	} else {
		std::ifstream ifs(input_file.c_str(), std::ios::in|std::ios::binary);
		if (!ifs.is_open()) {
			cerr << "could not open file: " << input_file << endl;
			return EXIT_FAILURE;
		}
		bool r = Load(&ifs, &units);
		ifs.close();
		if (!r) return EXIT_FAILURE;
	}

	ipc::TimeUnit tu;
	long d, n;
	for (UnitMap::const_iterator it=units.begin();it!=units.end();++it) {
		if (IsOfTime(units, it->first, &d, &n)) {
			if (vm.count("text") > 0) {
				printf("%d %s %ld %ld\n", it->first, it->second->name().c_str(), d, n);
			} else {
				tu.set_name(it->second->name());
				tu.set_d(d);
				tu.set_n(n);
				tu.set_id(it->first);
				if (!PackToOstream(tu, &std::cout)) {
					cerr << "failed to pack TimeUnit" << endl;
					return EXIT_FAILURE;
				}
			}
		}
	}

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
