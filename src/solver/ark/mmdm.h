/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_ARK_MMDM_H_
#define FLINT_SOLVER_ARK_MMDM_H_

#include <map>
#include <utility>

namespace flint {
namespace solver {
namespace ark {

/*
 * Mass-matrix data map.
 */
class Mmdm {
public:
	explicit Mmdm(long size);

	long size() const;

	void Add(long row, long col, long index);

	/*
	 * Find the data index for given pair (row, col).
	 * Return 0 if not found.
	 */
	long Find(long row, long col);

private:
	long size_;
	std::map<std::pair<long, long>, long> map_;
};

}
}
}

#endif
