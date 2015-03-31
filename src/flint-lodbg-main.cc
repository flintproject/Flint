/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/scoped_ptr.hpp>

#include "lo/layout.h"
#include "lo/layout_loader.h"

using std::cerr;
using std::cout;
using std::endl;

namespace {

void Usage()
{
	cerr << "usage: flint-lodbg LAYOUT" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	if (argc != 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if ( std::strcmp(argv[1], "-h") == 0 ||
		 std::strcmp(argv[1], "--help") == 0) {
		Usage();
		return EXIT_SUCCESS;
	}

	// load layout
	boost::scoped_ptr<Layout> layout(new Layout);
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(argv[1]));
		if (!loader->Load(layout.get())) return EXIT_FAILURE;
	}
	size_t layer_size = layout->Calculate();
	cout << "layer_size: " << layer_size << endl;
	layout->Debug(layer_size);

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
