/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
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
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"

#include "filter/filter_loader.h"
#include "isdf/isdf.h"

namespace po = boost::program_options;

using std::cerr;
using std::cin;
using std::endl;
using std::memcpy;
using std::string;
using std::vector;

namespace {

class Filter : boost::noncopyable {
public:
	Filter()
		: num_objs_(),
		  num_bytes_descs_(),
		  num_bytes_units_(),
		  descriptions_(),
		  units_()
	{}

	void ReadHeader(int /*size*/) const {
		// ignore header
	}

	void ReadColumn(lo::Column *column) {
		boost::uuids::uuid u;
		memcpy(&u, column->uuid().c_str(), 16);
		string s;
		if (u.is_nil()) {
			s = column->name();
		} else {
			s = to_string(u) + ":" + column->name();
			if (column->has_label()) s += "@" + column->label();
		}
		num_objs_ += column->size();
		num_bytes_descs_ += (sizeof(boost::uint32_t)+s.size()) * column->size();
		num_bytes_units_ += (sizeof(boost::uint32_t)+column->unit().size()) * column->size();
		for (int i=0;i<column->size();i++) {
			descriptions_.push_back(s);
			units_.push_back(column->unit());
		}
	}

	void Write(std::ofstream *ofs) const {
		isdf::ISDFHeader header;
		header.num_objs = num_objs_;
		header.num_bytes_comment = 0;
		header.num_bytes_descs = num_bytes_descs_;
		header.num_bytes_units = num_bytes_units_;

		char buf[sizeof(isdf::ISDFHeader)];
		memcpy(buf, &header, sizeof(header));
		ofs->write(buf, sizeof(header));

		for (vector<string>::const_iterator it=descriptions_.begin();it!=descriptions_.end();++it) {
			boost::uint32_t len = it->size();
			memcpy(buf, &len, sizeof(len));
			ofs->write(buf, sizeof(len));
			ofs->write(it->c_str(), len);
		}
		for (vector<string>::const_iterator it=units_.begin();it!=units_.end();++it) {
			boost::uint32_t len = it->size();
			memcpy(buf, &len, sizeof(len));
			ofs->write(buf, sizeof(len));
			ofs->write(it->c_str(), len);
		}
	}

private:
	boost::uint32_t num_objs_;
	boost::uint32_t num_bytes_descs_;
	boost::uint32_t num_bytes_units_;
	vector<string> descriptions_;
	vector<string> units_;
};

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string filter_file, output_file;
	int print_help = 0;

	opts.add_options()
		("filter", po::value<string>(&filter_file), "Filter file name")
		("output", po::value<string>(&output_file), "Output file")
		("help,h", "Show this message");
	popts.add("filter", 1).add("output", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) print_help = 1;
		if ( vm.count("filter") == 0 ||
			 vm.count("output") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " FILTER OUTPUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	// load filter
	boost::scoped_ptr<Filter> filter(new Filter());
	{
		boost::scoped_ptr<FilterLoader> loader(new FilterLoader(filter_file));
		if (!loader->Load(filter.get())) return EXIT_FAILURE;
	}
	std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
	if (!ofs.is_open()) {
		cerr << "could not open output file: " << output_file << endl;
		return EXIT_FAILURE;
	}
	filter->Write(&ofs);
	ofs.close();

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
