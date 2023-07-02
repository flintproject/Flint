/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CSV_EXPORT_H_
#define FLINT_CSV_EXPORT_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

namespace flint {

bool ExportIsdFromCsv(const boost::filesystem::path &input_path,
					  const boost::filesystem::path &output_path);

}

#endif
