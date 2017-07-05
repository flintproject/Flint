/* -*- Mode: C+; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILE_H_
#define FLINT_FILE_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

namespace flint {
namespace file {

enum Format {
	kCellml,
	kIsml,
	kMathml,
	kPhml,
	kPhsp,
	kPhz,
	kSbml,
	kSedml
};

/*
 * Note that expected filename's encoding is native.
 * Return true if detection suceeded without errors, otherwise false.
 */
bool DetectFormat(const char *filename, Format *format);

/*
 * Note that filename is encoded in UTF-8.
 * Return true in case of success, otherwise false.
 */
bool Txt(const char *filename, Format *format, const boost::filesystem::path &dir);

}
}

#endif
