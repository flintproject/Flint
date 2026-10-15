/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/uuid-generator.h"

#include <cstdint>

#include <boost/version.hpp>
#include <boost/uuid/name_generator.hpp>

namespace flint {

boost::uuids::uuid GenerateUuidForCellml(const char *name)
{
	// `uuid -v5 ns:URL http://www.cellml.org/cellml/`
	// => cef12f77-ba0f-5120-980d-e798bb62891d
#if BOOST_VERSION >= 108600
	// Boost >= 1.86 rewrote boost::uuids::uuid (it is no longer an aggregate),
	// so construct it from a byte array via its array constructor.
	static const std::uint8_t kCellmlUuidBytes[16] = {
		0xCE, 0xF1, 0x2F, 0x77, 0xBA, 0x0F, 0x51, 0x20,
		0x98, 0x0D, 0xE7, 0x98, 0xBB, 0x62, 0x89, 0x1D
	};
	static const boost::uuids::uuid kCellmlUuid(kCellmlUuidBytes);
#else
	// Older Boost: boost::uuids::uuid is an aggregate (uint8_t data[16]).
	static const boost::uuids::uuid kCellmlUuid = {
		0xCE, 0xF1, 0x2F, 0x77, 0xBA, 0x0F, 0x51, 0x20,
		0x98, 0x0D, 0xE7, 0x98, 0xBB, 0x62, 0x89, 0x1D
	};
#endif
	boost::uuids::name_generator gen(kCellmlUuid);
	return gen(name);
}

}
