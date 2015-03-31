/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CAS_SEXP_H_
#define FLINT_CAS_SEXP_H_

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>

// S-expression
class Sexp {
public:
	enum Tag {
		kInt,
		kDouble,
		kSymbol,
		kCons
	};

	static Sexp *Apply1(Sexp *op, Sexp *arg) {
		Sexp *s = new Sexp(op, NULL);
		s->Append(arg);
		return s;
	}

	static Sexp *Apply2(Sexp *op, Sexp *arg0, Sexp *arg1) {
		Sexp *s = new Sexp(op, NULL);
		s->Append(arg0);
		s->Append(arg1);
		return s;
	}

	static Sexp *CreateList2(Sexp *car, Sexp *cadr) {
		Sexp *cdr = new Sexp(cadr, NULL);
		return new Sexp(car, cdr);
	}

	static Sexp *CreateList3(Sexp *car, Sexp *cadr, Sexp *caddr)
	{
		Sexp *cdr = CreateList2(cadr, caddr);
		return new Sexp(car, cdr);
	}

	explicit Sexp(int i) : tag_(kInt), i_(i) {}
	explicit Sexp(double d) : tag_(kDouble), d_(d) {}
	explicit Sexp(const char *s) : tag_(kSymbol), s_(strdup(s)) {}
	explicit Sexp(Sexp *car, Sexp *cdr) : tag_(kCons), car_(car), cdr_(cdr) {
		assert(car);
	}

	~Sexp() {
		if (IsSymbol()) free(s_);
	}

	Tag tag() const {return tag_;}

	const char *s() const {assert(IsSymbol());return s_;}
	int i() const {assert(IsInt());return i_;}

	Sexp *Clone() const {
		switch (tag_) {
		case kInt:
			return new Sexp(i_);
			break;
		case kDouble:
			return new Sexp(d_);
			break;
		case kSymbol:
			return new Sexp(s_);
			break;
		case kCons:
			return new Sexp(car_->Clone(), cdr_->Clone());
			break;
		}
		assert(false);
		return NULL;
	}

	void Append(Sexp *last) {
		assert(tag_ == kCons);
		if (cdr_) {
			cdr_->Append(last);
		} else {
			cdr_ = new Sexp(last, NULL);
		}
	}

	bool IsInt() const {
		return tag_ == kInt;
	}

	bool IsDouble() const {
		return tag_ == kDouble;
	}

	bool IsSymbol() const {
		return tag_ == kSymbol;
	}

	bool IsCons() const {
		return tag_ == kCons;
	}

	const char *GetOperator() const {
		assert(tag_ == kCons);
		assert(car_->tag_ == kSymbol);
		return car_->s_;
	}

	Sexp *GetCar() const {
		assert(tag_ == kCons);
		return car_;
	}

	void SetCar(Sexp *car) {
		assert(tag_ == kCons);
		car_ = car;
	}

	Sexp *GetCdr() const {
		assert(tag_ == kCons);
		return cdr_;
	}

	void SetCdr(Sexp *cdr) {
		assert(tag_ == kCons);
		cdr_ = cdr;
	}

	size_t IsCase() const {
		if (std::strcmp(GetOperator(), "case") == 0) {
			size_t len = GetLength();
			assert(len > 1);
			return len - 1;
		} else {
			return 0;
		}
	}

	bool IsConditional() const {
		return std::strcmp(GetOperator(), "case-set") == 0;
	}

	bool IsEquation() const {
		return std::strcmp(GetOperator(), "eq") == 0;
	}

	void Print() const {
		using std::printf;

		switch (tag_) {
		case kInt:
			printf("%d", i_);
			break;
		case kDouble:
			printf("%g", d_);
			break;
		case kSymbol:
			printf("%s", s_);
			break;
		case kCons:
			putchar('(');
			car_->Print();
			Sexp *rest = cdr_;
			while (rest) {
				if (rest->tag_ == kCons) {
					putchar(' ');
					rest->car_->Print();
				} else {
					printf(" . ");
					rest->Print();
					break;
				}
				rest = rest->cdr_;
			}
			putchar(')');
			break;
		}
	}

	void Write(std::ostream &os) const {
		switch (tag_) {
		case kInt:
			os << i_;
			break;
		case kDouble:
			os << d_;
			break;
		case kSymbol:
			os << s_;
			break;
		case kCons:
			os << '(';
			car_->Write(os);
			Sexp *rest = cdr_;
			while (rest) {
				if (rest->tag_ == kCons) {
					os << ' ';
					rest->car_->Write(os);
				} else {
					os << " . ";
					rest->Write(os);
					break;
				}
				rest = rest->cdr_;
			}
			os << ')';
			break;
		}
	}

	size_t GetLength() const {
		assert(tag_ == kCons);
		size_t len = 1;
		Sexp *s = cdr_;
		while (s) {
			len++;
			s = s->cdr_;
		}
		return len;
	}

private:
	Tag tag_;
	union {
		int i_;
		double d_;
		char *s_;
		Sexp *car_;
	};
	Sexp *cdr_;
};

#endif
