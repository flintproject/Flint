/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cas/printer.h"

#include <cassert>

#include "variable.h"
#include "variable-map.h"

namespace flint {
namespace cas {

Printer::Printer() = default;

Printer::~Printer() = default;

bool Printer::Load(sqlite3 *db)
{
	vm_.reset(new VariableMap);
	return vm_->Load(db);
}

void Printer::set_uuid(const boost::uuids::uuid &uuid)
{
	uuid_ = uuid;
}

void Printer::operator()(const Compound &c)
{
	os_.put('(');
	os_ << c.keyword;
	for (const auto &expr : c.children) {
		os_.put(' ');
		boost::apply_visitor(*this, expr);
	}
	os_.put(')');
}

void Printer::operator()(const Identifier &s)
{
	const std::string &n(s.name);
	if (n.at(0) != '%' || n == "%time") {
		os_ << n;
		return;
	}
	std::string name = n.substr(1);
	const Variable *v = vm_->Find(uuid_, name);
	assert(v != nullptr);
	switch (v->type()) {
	case 's':
	case 'x':
		os_ << n;
		break;
	case 'v':
		os_ << n << "#0";
		break;
	default:
		assert(false);
		break;
	}
}

void Printer::operator()(int i)
{
	os_ << i;
}

void Printer::operator()(const flint::lexer::Real &r)
{
	os_ << r.lexeme;
}

std::string Printer::GetAndClearString()
{
	std::string s = os_.str();
	os_.str("");
	return s;
}

}
}
