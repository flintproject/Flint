/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_BC_MOUNTER_H_
#define FLINT_BC_MOUNTER_H_

#include <memory>
#include <boost/noncopyable.hpp>

namespace flint {

class Mounter : boost::noncopyable {
public:
	explicit Mounter(int size) : size_(size), offsets_(new int[size]) {}

	int size() const {return size_;}

	void SetOffset(int i, int offset) {
		assert(i < size_);
		offsets_[i] = offset;
	}

	int GetOffset(int i) const {
		assert(i < size_);
		return offsets_[i];
	}

private:
	int size_;
	std::unique_ptr<int[]> offsets_;
};

}

#endif
