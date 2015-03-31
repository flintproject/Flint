/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <string>

#include <boost/program_options.hpp>
#include <boost/scoped_ptr.hpp>

#include "db/driver.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::printf;
using std::string;

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string input_file;
	int print_help = 0;

	opts.add_options()
		("input", po::value<string>(&input_file), "Input file name")
		("help,h", "Show this message");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
		else if (vm.count("input") == 0) print_help = 2;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " DB" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	boost::scoped_ptr<db::Driver> driver(new db::Driver(input_file.c_str()));
	sqlite3_stmt *stmt;
	int e = sqlite3_prepare_v2(driver->db(),
							   "SELECT m.module_id||i.math FROM ivs AS i LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
							   -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << e << endl;
		return EXIT_FAILURE;
	}
	for (e = sqlite3_step(stmt); e == SQLITE_ROW; e = sqlite3_step(stmt)) {
		const unsigned char *line = sqlite3_column_text(stmt, 0);
		printf("%s\n", (const char *)line);
	}
	sqlite3_finalize(stmt);

	return EXIT_SUCCESS;
}
