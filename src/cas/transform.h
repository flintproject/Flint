/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CAS_TRANSFORM_H_
#define FLINT_CAS_TRANSFORM_H_

#include <cassert>
#include <cstdio>
#include <cstring>

#include <boost/noncopyable.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/uuid/uuid.hpp>

#include "cas/sexp.h"

namespace {

Sexp *GetLhs(Sexp *statement)
{
	return statement->GetCdr()->GetCar();
}

Sexp *GetRhs(Sexp *statement)
{
	return statement->GetCdr()->GetCdr()->GetCar();
}

const char *GetId(Sexp *lhs)
{
	Sexp *s = lhs->GetCdr()->GetCdr()->GetCar();
	assert(s);
	assert(s->IsSymbol());
	return s->s();
}

void ReportErrorAndDie(const char *uuid)
{
	std::fprintf(stderr, "equation expected, but invalid formula found in a <case-set>: %s\n", uuid);
	std::exit(EXIT_FAILURE);
}

Sexp *TransformConditionalWithPiecewise(const char *uuid, Sexp *conditional)
{
	Sexp *s = new Sexp(new Sexp("piecewise"), NULL);
	Sexp *rest = conditional->GetCdr();
	Sexp *lhs = NULL;
	while (rest) {
		Sexp *c = rest->GetCar();
		size_t i = c->IsCase();
		switch (i) {
		case 1:
			{
				Sexp *v = c->GetCdr()->GetCar();
				if (v->IsConditional()) {
					v = TransformConditionalWithPiecewise(uuid, v);
				}
				if (!v->IsEquation()) ReportErrorAndDie(uuid);
				lhs = GetLhs(v);

				Sexp *o = Sexp::Apply1(new Sexp("otherwise"), GetRhs(v));
				s->Append(o);
			}
			break;
		case 2:
			{
				Sexp *a = c->GetCdr()->GetCar();
				Sexp *v = c->GetCdr()->GetCdr()->GetCar();
				if (v->IsConditional()) {
					v = TransformConditionalWithPiecewise(uuid, v);
				}
				if (!v->IsEquation()) ReportErrorAndDie(uuid);
				lhs = GetLhs(v);

				Sexp *p = Sexp::Sexp::Apply2(new Sexp("piece"), GetRhs(v), a);
				s->Append(p);
			}
			break;
		default:
			assert(false);
			break;
		}
		rest = rest->GetCdr();
	}
	return Sexp::Apply2(new Sexp("eq"), lhs, s);
}

} // namespace

class AssignmentTransformer : boost::noncopyable {
public:
	bool Transform(const char *uuid, Sexp *statement, boost::ptr_vector<Ast> *asts) {
		if (statement->IsConditional()) {
			Sexp *s = TransformConditionalWithPiecewise(uuid, statement);
			return Transform(uuid, s, asts);
		} else if (statement->IsEquation()) {
			Sexp *lhs = GetLhs(statement);
			Sexp *rhs = GetRhs(statement);
			if (lhs->IsSymbol()) {
				const char *id = lhs->s();
				boost::scoped_array<char> id0(new char[std::strlen(id) + 3]);
				std::sprintf(id0.get(), "%s#0", id);
				asts->push_back(new Ast(id0.get(), ReplaceParameter(rhs)));
				return true;
			} else {
				assert(false);
			}
		} else {
			assert(false);
		}
		return false;
	}

private:
	Sexp *ReplaceParameter(Sexp *s) {
		if (s) {
			switch (s->tag()) {
			case Sexp::kInt:
			case Sexp::kDouble:
				return s->Clone();
			case Sexp::kSymbol:
				{
					if (*s->s() != '%') {
						return s->Clone();
					} else if (strcmp(s->s(), "%time") == 0) {
						return s->Clone();
					} else {
						const char *sname = s->s();
						boost::scoped_array<char> buf(new char[std::strlen(sname) + 3]);
						std::sprintf(buf.get(), "%s#0", sname);
						return new Sexp(buf.get());
					}
				}
			case Sexp::kCons:
				{
					Sexp *car = ReplaceParameter(s->GetCar());
					Sexp *cdr = ReplaceParameter(s->GetCdr());
					return new Sexp(car, cdr);
				}
			}
		}
		return NULL;
	}
};

class EventTransformer : boost::noncopyable {
public:
	bool Transform(const char *uuid, Sexp *statement, boost::ptr_vector<Ast> *asts) {
		if (statement->IsConditional()) {
			Sexp *s = TransformConditionalWithPiecewise(uuid, statement);
			return Transform(uuid, s, asts);
		} else if (statement->IsEquation()) {
			Sexp *lhs = GetLhs(statement);
			Sexp *rhs = GetRhs(statement);
			if (lhs->IsSymbol()) {
				const char *id = lhs->s();
				boost::scoped_array<char> id0(new char[std::strlen(id) + 3]);
				std::sprintf(id0.get(), "%s#0", id);
				asts->push_back(new Ast(id0.get(), rhs));
				return true;
			} else {
				assert(false);
			}
		} else {
			assert(false);
		}
		return false;
	}
};

class EulerTransformer : boost::noncopyable {
public:
	bool Transform(const char *uuid, Sexp *statement, boost::ptr_vector<Ast> *asts) {
		if (statement->IsConditional()) {
			Sexp *s = TransformConditionalWithPiecewise(uuid, statement);
			return Transform(uuid, s, asts);
		} else if (statement->IsEquation()) {
			Sexp *lhs = GetLhs(statement);
			Sexp *rhs = GetRhs(statement);
			if (lhs->IsSymbol()) {
				const char *id = lhs->s();
				boost::scoped_array<char> id0(new char[std::strlen(id) + 3]);
				std::sprintf(id0.get(), "%s#0", id);
				asts->push_back(new Ast(id0.get(), ReplaceParameter(rhs)));
				return true;
			} else {
				const char *id = GetId(lhs);
				boost::scoped_array<char> id0(new char[std::strlen(id) + 3]);
				std::sprintf(id0.get(), "%s#0", id);
				Sexp *s0 = Sexp::Apply2(new Sexp("times"),
										new Sexp("@dt"),
										rhs);
				Sexp *s1 = Sexp::Apply2(new Sexp("plus"),
										new Sexp(id),
										s0);
				asts->push_back(new Ast(id0.get(), s1));
				return true;;
			}
		} else {
			assert(false);
		}
		return false;
	}

private:
	Sexp *ReplaceParameter(Sexp *s) {
		if (s) {
			switch (s->tag()) {
			case Sexp::kInt:
			case Sexp::kDouble:
				return s->Clone();
			case Sexp::kSymbol:
				{
					if (*s->s() != '%') {
						return s->Clone();
					} else if (strcmp(s->s(), "%time") == 0) {
						return Sexp::Apply2(new Sexp("plus"),
											new Sexp("%time"),
											new Sexp("@dt"));
					} else {
						const char *sname = s->s();
						boost::scoped_array<char> buf(new char[std::strlen(sname) + 3]);
						std::sprintf(buf.get(), "%s#0", sname);
						return new Sexp(buf.get());
					}
				}
			case Sexp::kCons:
				{
					Sexp *car = ReplaceParameter(s->GetCar());
					Sexp *cdr = ReplaceParameter(s->GetCdr());
					return new Sexp(car, cdr);
				}
			}
		}
		return NULL;
	}
};

class Rk4Transformer : boost::noncopyable {
public:
	bool Transform(const char *uuid, Sexp *statement, boost::ptr_vector<Ast> *asts) {
		using std::sprintf;
		using std::strlen;

		if (statement->IsConditional()) {
			Sexp *s = TransformConditionalWithPiecewise(uuid, statement);
			return Transform(uuid, s, asts);
		} else if (statement->IsEquation()) {
			Sexp *lhs = GetLhs(statement);
			Sexp *rhs = GetRhs(statement);
			if (lhs->IsSymbol()) {
				const char *id = lhs->s();
				size_t len = strlen(id) + 16; // FIXME
				boost::scoped_array<char> id0(new char[len]);
				sprintf(id0.get(), "%s#0", id);
				boost::scoped_array<char> id2(new char[len]);
				sprintf(id2.get(), "%s#2", id);
				boost::scoped_array<char> id4(new char[len]);
				sprintf(id4.get(), "%s#4", id);
				boost::scoped_array<char> id6(new char[len]);
				sprintf(id6.get(), "%s#6", id);

				asts->push_back(new Ast(id2.get(), ReplaceVariable(rhs, 2, 2)));
				asts->push_back(new Ast(id4.get(), ReplaceVariable(rhs, 4, 2)));
				asts->push_back(new Ast(id6.get(), ReplaceVariable(rhs, 6, 1)));
				asts->push_back(new Ast(id0.get(), ReplaceVariable(rhs, 0, 1)));
				return true;
			} else {
				const char *id = GetId(lhs);
				size_t len = strlen(id) + 16; // FIXME
				boost::scoped_array<char> id0(new char[len]);
				sprintf(id0.get(), "%s#0", id);
				boost::scoped_array<char> k1(new char[len]);
				sprintf(k1.get(), "%s#1", id);
				boost::scoped_array<char> y1(new char[len]);
				sprintf(y1.get(), "%s#2", id);
				boost::scoped_array<char> k2(new char[len]);
				sprintf(k2.get(), "%s#3", id);
				boost::scoped_array<char> y2(new char[len]);
				sprintf(y2.get(), "%s#4", id);
				boost::scoped_array<char> k3(new char[len]);
				sprintf(k3.get(), "%s#5", id);
				boost::scoped_array<char> y3(new char[len]);
				sprintf(y3.get(), "%s#6", id);
				boost::scoped_array<char> k4(new char[len]);
				sprintf(k4.get(), "%s#7", id);

				// #1: k1 = dt * f(t_n, y_n)
				asts->push_back(new Ast(k1.get(), Sexp::Apply2(new Sexp("times"),
																  new Sexp("@dt"),
																  rhs)));
				// #2: y1 = y_n + k1/2
				asts->push_back(new Ast(y1.get(), Sexp::Apply2(new Sexp("plus"),
																  new Sexp(id),
																  Sexp::Apply2(new Sexp("divide"),
																			   new Sexp(k1.get()),
																			   new Sexp(2)))));

				// #3: k2 = dt * f(t_n + dt/2, y1)
				asts->push_back(new Ast(k2.get(), Sexp::Apply2(new Sexp("times"),
																  new Sexp("@dt"),
																  ReplaceVariable(rhs, 2, 2))));
				// #4: y2 = y_n + k2/2
				asts->push_back(new Ast(y2.get(), Sexp::Apply2(new Sexp("plus"),
																  new Sexp(id),
																  Sexp::Apply2(new Sexp("divide"),
																			   new Sexp(k2.get()),
																			   new Sexp(2)))));

				// #5: k3 = dt * f(t_n + dt/2, y2)
				asts->push_back(new Ast(k3.get(), Sexp::Apply2(new Sexp("times"),
																  new Sexp("@dt"),
																  ReplaceVariable(rhs, 4, 2))));
				// #6: y3 = y_n + k3
				asts->push_back(new Ast(y3.get(), Sexp::Apply2(new Sexp("plus"),
																  new Sexp(id),
																  new Sexp(k3.get()))));

				// #7: k4 = dt * f(t_n + dt, y3)
				asts->push_back(new Ast(k4.get(), Sexp::Apply2(new Sexp("times"),
																  new Sexp("@dt"),
																  ReplaceVariable(rhs, 6, 1))));

				// #0: y_(n+1) = y_n + (k1 + 2*k2 + 2*k3 + k4)/6
				Sexp *s22 = Sexp::Apply2(new Sexp("times"),
										 new Sexp(2),
										 new Sexp(k2.get()));
				Sexp *s23 = Sexp::Apply2(new Sexp("times"),
										 new Sexp(2),
										 new Sexp(k3.get()));

				Sexp *sum34 = Sexp::Apply2(new Sexp("plus"),
										   s23,
										   new Sexp(k4.get()));
				Sexp *sum234 = Sexp::Apply2(new Sexp("plus"),
											s22,
											sum34);
				Sexp *sum1234 = Sexp::Apply2(new Sexp("plus"),
											 new Sexp(k1.get()),
											 sum234);

				Sexp *sd = Sexp::Apply2(new Sexp("divide"),
										sum1234,
										new Sexp(6));
				Sexp *sp = Sexp::Apply2(new Sexp("plus"),
										new Sexp(id),
										sd);
				asts->push_back(new Ast(id0.get(), sp));
				return true;
			}
		} else {
			assert(false);
		}
		return false;
	}

private:
	Sexp *ReplaceVariable(Sexp *s, int k, int n) {
		if (s) {
			switch (s->tag()) {
			case Sexp::kInt:
			case Sexp::kDouble:
				return s->Clone();
			case Sexp::kSymbol:
				{
					if (*s->s() != '%') {
						return s->Clone();
					} else if (strcmp(s->s(), "%time") == 0) {
						if (n == 1) {
							return Sexp::Apply2(new Sexp("plus"),
												new Sexp("%time"),
												new Sexp("@dt"));
						} else {
							return Sexp::Apply2(new Sexp("plus"),
												new Sexp("%time"),
												Sexp::Apply2(new Sexp("divide"),
															 new Sexp("@dt"),
															 new Sexp(n)));
						}
					} else {
						const char *sname = s->s();
						boost::scoped_array<char> buf(new char[std::strlen(sname) + 16]);
						std::sprintf(buf.get(), "%s#%d", sname, k);
						return new Sexp(buf.get());
					}
				}
			case Sexp::kCons:
				{
					Sexp *car = ReplaceVariable(s->GetCar(), k, n);
					Sexp *cdr = ReplaceVariable(s->GetCdr(), k, n);
					return new Sexp(car, cdr);
				}
			}
		}
		return NULL;
	}
};

template<typename TTransformer>
void ProcessLine(TTransformer *transformer, const char *uuid, Sexp *s) {
	boost::ptr_vector<Ast> asts;
	bool b = transformer->Transform(uuid, s, &asts);
	assert(b);
	for (boost::ptr_vector<Ast>::const_iterator it=asts.begin();it!=asts.end();++it) {
		it->Print(uuid);
	}
	std::fflush(stdout);
}

#endif
