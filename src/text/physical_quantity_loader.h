/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TEXT_PHYSICAL_QUANTITY_LOADER_H_
#define FLINT_TEXT_PHYSICAL_QUANTITY_LOADER_H_

#include <fstream>
#include <iostream>

#include <boost/noncopyable.hpp>
#include <boost/scoped_array.hpp>
#include <boost/uuid/uuid_generators.hpp>

namespace text {

class PhysicalQuantityLoader : boost::noncopyable {
public:
	explicit PhysicalQuantityLoader(const char *file) : ifs_(file, std::ios::in), gen_() {}

	~PhysicalQuantityLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		using std::cerr;
		using std::endl;

		static const int kUuidSize = 36;
		static const int kLineSize = 1024; // TODO

		if (!ifs_.is_open()) {
			cerr << "failed to open physical-quantity file" << endl;
			return false;
		}

		boost::scoped_array<char> line(new char[kLineSize]);
		while (ifs_.getline(line.get(), kLineSize)) {
			size_t len = strlen(line.get());
			if (len <= kUuidSize+3) {
				cerr << "invalid line: " << line.get() << endl;
				return false;
			}
			switch (line[kUuidSize+1]) {
			case 's':
			case 't':
			case 'v':
			case 'x':
				/* OK */
				break;
			default:
				cerr << "invalid type of physical-quantity: " << line.get() << endl;
				return false;
			}
			int pq_id = 0;
			boost::scoped_array<char> name(new char[len-kUuidSize]);
			double capacity = 0;
			if (sscanf(line.get()+kUuidSize+3, "%d%s%lf", &pq_id, name.get(), &capacity) < 2) {
				cerr << "invalid line: " << line.get() << endl;
				return false;
			}
			if (pq_id <= 0) {
				cerr << "invalid physical-quantity-id: " << line.get() << endl;
				return false;
			}
			if (strlen(name.get()) == 0) {
				cerr << "invalid physical-quantity name: " << line.get() << endl;
				return false;
			}
			line[kUuidSize] = '\0';
			if (!handler->Handle(gen_(line.get()), line[kUuidSize+1], pq_id, name.get(), capacity)) return false;
		}
		return true;
	}

private:
	std::ifstream ifs_;
	boost::uuids::string_generator gen_;
};

} // namespace text

#endif
