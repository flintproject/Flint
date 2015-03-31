/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <vector>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/scoped_ptr.hpp>

#include "lo.pb.h"

#include "bc/index.h"
#include "db/driver.h"
#include "db/reach-loader.h"
#include "lo/layout_loader.h"

using std::cerr;
using std::cout;
using std::endl;
using std::make_pair;
using std::pair;
using std::map;
using std::string;
using std::vector;

namespace {

class Layout : boost::noncopyable {
public:
	Layout() : offset_(kOffsetBase), sector_size_(), sector_index_() {}

	void AddTrack(lo::Track *track) {
		offset_ += sector_size_ * sector_index_;
		sector_size_ = 0;
		sector_index_ = 0;
		data_.clear();
		delete track;
	}

	void AddData(lo::Data *data) {
		int id = data->id();
		int size = data->size();
		data_.push_back(make_pair(id, size));
		sector_size_ += size;
		delete data;
	}

	void AddSector(lo::Sector *sector) {
		string id = sector->id();
		int sector_offset = 0;
		for (vector<pair<int, int> >::const_iterator it=data_.begin();it!=data_.end();++it) {
			m_[id][it->first] = offset_ + sector_offset + (sector_index_ * sector_size_);
			sector_offset += it->second;
		}
		sector_index_++;
		delete sector;
	}

	int GetOffset(const string &uuid, int id) const {
		const OffsetMap &om(m_.at(uuid));
		OffsetMap::const_iterator it = om.find(id);
		if (it == om.end()) return -1; // error
		return it->second;
	}

private:
	typedef map<int, int> OffsetMap;
	typedef boost::ptr_map<string, OffsetMap> SectorMap;

	int offset_;
	int sector_size_;
	int sector_index_;
	vector<pair<int, int> > data_;
	SectorMap m_;
};

class ReachHandler : boost::noncopyable {
public:
	explicit ReachHandler(Layout *layout)
	: layout_(layout)
	{
	}

	bool Handle(const void *output_uuid, int output_id,
				const void *input_uuid, int input_id) {
		string s0((const char *)output_uuid, 16);
		string s1((const char *)input_uuid, 16);
		int o0 = layout_->GetOffset(s0, output_id);
		int o1 = layout_->GetOffset(s1, input_id);
		printf("%d %d\n", o0, o1);
		return true;
	}

private:
	Layout *layout_;
};

void Usage()
{
	cerr << "flint-flow DB LAYOUT" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	if (argc == 2) {
		Usage();
		if ( strcmp("-h", argv[1]) == 0 ||
			 strcmp("--help", argv[1]) == 0 ) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}
	if (argc != 3) {
		Usage();
		return EXIT_FAILURE;
	}

	// load layout at first
	boost::scoped_ptr<Layout> layout(new Layout);
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(argv[2]));
		if (!loader->Load(layout.get())) return EXIT_FAILURE;
	}

	boost::scoped_ptr<db::Driver> driver(new db::Driver(argv[1]));
	{
		boost::scoped_ptr<db::ReachLoader> loader(new db::ReachLoader(driver->db()));
		boost::scoped_ptr<ReachHandler> handler(new ReachHandler(layout.get()));
		if (!loader->Load(handler.get())) return EXIT_FAILURE;
	}

	google::protobuf::ShutdownProtobufLibrary();
	return EXIT_SUCCESS;
}
