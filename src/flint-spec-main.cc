/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>

#include <boost/program_options.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"

#include "bc/index.h"
#include "lo/layout_loader.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::printf;
using std::sscanf;
using std::string;
using std::strlen;

namespace {

class Layout : boost::noncopyable {
public:
	void AddTrack(lo::Track *track) {
		tv_.push_back(track);
	}

	void AddSector(lo::Sector *sector) {
		sv_.push_back(sector);
	}

	void AddData(lo::Data *data) {
		dv_.push_back(data);
	}

	int PrintSpecs() const {
		string us;
		boost::uuids::uuid u;

		int si = 0;
		int di = 0;
		int pos = kOffsetBase;
		for (TrackVector::const_iterator it=tv_.begin();it!=tv_.end();++it) {
			int nos = it->nos();
			int nod = it->nod();
			int sie = si + nos;
			int dib = di;
			int die = di + nod;

			while (si < sie) {
				const lo::Sector &s = sv_.at(si++);
				di = dib;
				while (di < die) {
					const lo::Data &d = dv_.at(di++);
					switch (d.type()) {
					case lo::X:
					case lo::V:
						memcpy(&u, s.id().c_str(), 16);
						us = to_string(u);
						printf("%s %s\n", us.c_str(), d.name().c_str());
						break;
					default:
						// skip it
						break;
					}
					pos += d.size();
				}
			}
		}
		return pos;
	}

private:
	typedef boost::ptr_vector<lo::Track> TrackVector;
	typedef boost::ptr_vector<lo::Sector> SectorVector;
	typedef boost::ptr_vector<lo::Data> DataVector;

	TrackVector tv_;
	SectorVector sv_;
	DataVector dv_;
};

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string layout_file;
	int print_help = 0;

	opts.add_options()
		("input", po::value<string>(&layout_file), "Input layout file")
		("help,h", "Show this message");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " LAYOUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	Layout layout;
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(layout_file.c_str()));
		if (!loader->Load(&layout)) return EXIT_FAILURE;
	}
	layout.PrintSpecs();

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
