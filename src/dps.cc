/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/dps.h"

namespace flint {
namespace dps {

Cursor::Cursor(boost::interprocess::file_mapping &fm, size_t offset, size_t row_size)
	: mr_(fm, boost::interprocess::read_only, offset)
	, row_size_(row_size)
	, position_(0)
{
	mr_.advise(boost::interprocess::mapped_region::advice_sequential);
}

Cursor::Position Cursor::operator()(double t, double **p)
{
	if (position_ >= mr_.get_size())
		return Position::kEnd;
	double *h = reinterpret_cast<double *>(reinterpret_cast<char *>(mr_.get_address()) + position_);
	double t0 = *h;
	if (t < t0)
		return Position::kGt;
	position_ += row_size_;
	*p = h;
	return Position::kLeq;
}

}
}
