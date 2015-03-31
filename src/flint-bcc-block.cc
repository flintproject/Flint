/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bc/pack.h"

#include "flint-bcc-block.h"

Block::Block() : v_(), l_(), uuid_(NULL), name_(NULL), nod_() {}

Block::~Block()
{
	free(uuid_);
	free(name_);
}

int Block::GetCodeSize() const
{
	return static_cast<int>(v_.size());
}

void Block::Add(bc::Code *code)
{
	v_.push_back(code);
}

void Block::SaveLabel(int label)
{
	if (l_.size() <= static_cast<size_t>(label)) l_.resize(label+1);
	l_.at(label) = static_cast<int>(v_.size());
}

void Block::LookupLabels()
{
	int l;
	for (CodeVector::iterator it=v_.begin();it!=v_.end();++it) {
		switch (it->type()) {
		case bc::Code::kBr:
			{
				bc::Br *br = it->mutable_br();
				l = br->l();
				br->clear_l();
				br->set_p(l_.at(l));
			}
			break;
		case bc::Code::kJmp:
			{
				bc::Jmp *jmp = it->mutable_jmp();
				l = jmp->l();
				jmp->clear_l();
				jmp->set_p(l_.at(l));
			}
			break;
		default:
			break;
		}
	}
}

void Block::Print() const
{
	for (CodeVector::const_iterator it=v_.begin();it!=v_.end();++it) {
		PackToOstream(*it, &std::cout);
	}
}
