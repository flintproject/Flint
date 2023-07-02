/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>

#include "lo/layout.h"
#include "lo/layout_loader.h"

using namespace flint;

namespace {

void Usage()
{
	std::cerr << "usage: flint-lodbg LAYOUT" << std::endl;
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
	std::unique_ptr<Layout> layout(new Layout);
	{
		std::unique_ptr<LayoutLoader> loader(new LayoutLoader(argv[1]));
		if (!loader->Load(layout.get())) return EXIT_FAILURE;
	}
	size_t layer_size = layout->Calculate();
	std::cout << "layer_size: " << layer_size << std::endl;
	layout->Debug(layer_size);

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
