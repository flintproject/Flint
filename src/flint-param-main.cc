/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <set>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"

#include "bc/binary.h"
#include "bc/index.h"
#include "lo/layout_loader.h"

namespace po = boost::program_options;

using std::cerr;
using std::cout;
using std::endl;
using std::memcpy;
using std::set;
using std::string;

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

	int Fill(boost::ptr_vector<lo::Column> *columns) const {
		std::auto_ptr<lo::Column> c(new lo::Column);
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
					case lo::S:
					case lo::X:
						c.reset(new lo::Column);
						c->set_position(pos);
						c->set_size(d.size());
						memcpy(&u, s.id().c_str(), 16);
						c->set_uuid(to_string(u));
						c->set_id(d.id());
						c->set_name(d.name());
						c->set_type(d.type());
						c->set_unit(d.unit());
						if (s.has_label()) c->set_label(s.label());
						if (it->has_name()) c->set_track_name(it->name());
						columns->push_back(c.release());
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

	RequestBinaryStdio();

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string layout_file;
	int print_help = 0;

	opts.add_options()
		("layout", po::value<string>(&layout_file), "Input layout file")
		("help,h", "Show this message");
	popts.add("layout", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if (vm.count("layout") == 0) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " LAYOUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	boost::scoped_ptr<Layout> layout(new Layout);
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(layout_file));
		if (!loader->Load(layout.get())) return EXIT_FAILURE;
	}

	boost::scoped_ptr<boost::ptr_vector<lo::Column> > columns(new boost::ptr_vector<lo::Column>);
	int size = layout->Fill(columns.get());
	boost::scoped_ptr<lo::Header> header(new lo::Header);
	header->set_size(size);
	if (!PackToOstream(*header, &cout)) {
		cerr << "failed to pack Header" << endl;
		return EXIT_FAILURE;
	}
	for (boost::ptr_vector<lo::Column>::const_iterator it=columns->begin();it!=columns->end();++it) {
		if (!PackToOstream(*it, &cout)) {
			cerr << "failed to pack Column" << endl;
			return EXIT_FAILURE;
		}
	}

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
