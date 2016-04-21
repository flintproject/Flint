/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_ISDF_ISDF_H_
#define FLINT_ISDF_ISDF_H_

#include <cstdint>
#include <cstring>
#include <ctime>

namespace flint {
namespace isdf {

/*
 * insilicoSim binary Data output Format
 *
 *    TODO: Matrix or more complex data value handling
 *    TODO: More sophisticated header, containing simulation setup information etc.
 *    TODO: support for dynamicaly adding or deleting objects
 */

#pragma pack(1)
typedef struct ISDFHeaderStruct{
        char magic[4];             // must be 'I' 'S' 'D' 'F'
        char version;              // currently, 1
        char little_endian;        // 1 if little endian file
        char padding[2];           // should be 0
	char timestamp[20];
	std::uint32_t num_objs;     // number of objects recorded in each step
	std::uint32_t num_bytes_comment; // number of bytes for comment
	std::uint32_t num_bytes_descs; // number of bytes for descriptions
	std::uint32_t num_bytes_units; // number of bytes for units

	ISDFHeaderStruct() {
		magic[0] = 'I'; magic[1] = 'S';
		magic[2] = 'D'; magic[3] = 'F';
		version = 1;
#if defined(__LITTLE_ENDIAN__) || __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
		little_endian = 1;
#elif defined(__BIG_ENDIAN__) ||  __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
		little_endian = 0;
#else
		{
			int i = 1;
			little_endian = *(char*)&i;
        }
#endif
		padding[0] = padding[1] = 0;

		time_t t = std::time(nullptr);
		struct tm *utc = std::gmtime(&t);
		// Note that on Windows strftime() comes from MSVCRT.LIB,
		// so some of the formatting codes are unavailable.
		if (std::strftime(timestamp, 20, "%Y-%m-%dT%H:%M:%S", utc) == 0) {
			// put the epoch as a last resort
			std::strcpy(timestamp, "1970-01-01T00:00:00");
		}
		timestamp[19] = 'Z';
	}
} ISDFHeader;
#pragma pack()

}
}

#endif // ISDF_HEADER_
