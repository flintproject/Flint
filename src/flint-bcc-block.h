/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef BLOCK_H_
#define BLOCK_H_

#include <vector>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include "bc.pb.h"

class Block : boost::noncopyable {
public:
	Block();
	~Block();

	const char *uuid() const {return uuid_;}
	void set_uuid(char *uuid) {uuid_ = uuid;}
	const char *name() const {return name_;}
	void set_name(char *name) {name_ = name;}
	int nod() const {return nod_;}
	void set_nod(int nod) {nod_ = nod;}

	int GetCodeSize() const;

	void Add(bc::Code *code);

	void SaveLabel(int label);

	void LookupLabels();

	void Print() const;

private:
	typedef boost::ptr_vector<bc::Code> CodeVector;

	CodeVector v_;
	std::vector<int> l_;
	char *uuid_;
	char *name_;
	int nod_;
};

#endif
