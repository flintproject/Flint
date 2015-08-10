/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LAYOUT_COPIER_HH_
#define FLINT_LAYOUT_COPIER_HH_

#include <utility>
#include <vector>

#include <boost/noncopyable.hpp>

namespace flint {
namespace layout {

class Copier : boost::noncopyable {
public:
	explicit Copier(const std::vector<int> &offsets);

	void Copy(const double *source, double *target) const;

private:
	std::vector<std::pair<size_t, size_t> > v_;
};

}
}

#endif
