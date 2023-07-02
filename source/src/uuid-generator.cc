/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/uuid-generator.h"

#include <boost/uuid/name_generator.hpp>

namespace flint {

boost::uuids::uuid GenerateUuidForCellml(const char *name)
{
	static const boost::uuids::uuid kCellmlUuid = {
		// `uuid -v5 ns:URL http://www.cellml.org/cellml/`
		// => cef12f77-ba0f-5120-980d-e798bb62891d
		0xCE, 0xF1, 0x2F, 0x77, 0xBA, 0x0F, 0x51, 0x20,
		0x98, 0x0D, 0xE7, 0x98, 0xBB, 0x62, 0x89, 0x1D
	};
	boost::uuids::name_generator gen(kCellmlUuid);
	return gen(name);
}

}
