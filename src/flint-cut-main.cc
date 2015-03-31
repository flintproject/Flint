/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include "lo.pb.h"

#include "bc/binary.h"
#include "filter/filter_loader.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::make_pair;
using std::map;
using std::string;

namespace {

class Filter : boost::noncopyable {
public:
	Filter() : size_(), columns_() {}

	void ReadHeader(int size) {
		size_ = size;
	}

	void ReadColumn(lo::Column *column) {
		columns_.insert(make_pair(column->position(), column->size()));
		delete column;
	}

	bool Apply() const {
		if (size_ == 0) {
			cerr << "size is zero" << endl;
			return false;
		}
		boost::scoped_array<double> data(new double[size_]);
		for (;;) {
			size_t s = fread(data.get(), sizeof(double), size_, stdin);
			if (s == 0) break;
			if (s != size_) {
				cerr << "too short input" << endl;
				return false;
			}
			for (map<int, int>::const_iterator it=columns_.begin();it!=columns_.end();++it) {
				size_t p = static_cast<size_t>(it->first);
				s = static_cast<size_t>(it->second);
				if (fwrite(data.get()+p, sizeof(double), s, stdout) != s) {
					cerr << "failed to filter output" << endl;
					return false;
				}
			}
		}
		return true;
	}

private:
	size_t size_;
	map<int, int> columns_;
};

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	RequestBinaryStdio();

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string filter_file;
	int print_help = 0;

	opts.add_options()
		("filter", po::value<string>(&filter_file), "Input filter file")
		("help,h", "Show this message");
	popts.add("filter", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if (vm.count("filter") == 0) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " FILTER" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	// load filter
	boost::scoped_ptr<Filter> filter(new Filter);
	{
		boost::scoped_ptr<FilterLoader> loader(new FilterLoader(filter_file));
		if (!loader->Load(filter.get())) return EXIT_FAILURE;
	}
	if (!filter->Apply()) return EXIT_FAILURE;

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
