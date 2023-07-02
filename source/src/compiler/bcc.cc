/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bcc.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <boost/uuid/uuid_io.hpp>

#include "bc.pb.h"
#include "flint/bc.h"
#include "compiler/bcc/parser.h"

namespace flint {
namespace compiler {
namespace bcc {

namespace {

struct Block {
	int GetCodeSize() const {
		return static_cast<int>(code.size());
	}

	std::vector<bc::Code> code;
	boost::uuids::uuid uuid;
	std::string name;
	int noir;
	int nod;
};

void ProcessBody(Block &block, Body &body)
{
	int l;
	for (auto &c : body.code) {
		switch (c.type()) {
		case bc::Code::kBr:
			{
				bc::Br *br = c.mutable_br();
				l = br->l();
				br->clear_l();
				br->set_p(body.labels.at(l));
			}
			break;
		case bc::Code::kJmp:
			{
				bc::Jmp *jmp = c.mutable_jmp();
				l = jmp->l();
				jmp->clear_l();
				jmp->set_p(body.labels.at(l));
			}
			break;
		default:
			break;
		}
	}
	block.code.swap(body.code);
}

typedef std::vector<Block> BlockVector;

class BlockParser {
public:
	explicit BlockParser(BlockVector *bv)
		: bv_(bv)
	{
	}

	int Parse(const boost::uuids::uuid &uuid, const char *name, int noir, int nod, const char *code) {
		Parser parser(code);
		Body body;
		if (!parser(&body)) {
			std::cerr << "failed to parse: " << code << std::endl;
			return 1;
		}
		Block block;
		block.uuid = uuid;
		block.name = name;
		block.noir = noir;
		block.nod = nod;
		ProcessBody(block, body);
		bv_->push_back(block);
		return 0;
	}

private:
	BlockVector *bv_;
};

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	BlockParser *parser = static_cast<BlockParser *>(data);
	assert(argc == 5);
	assert(argv[0]);
	boost::uuids::uuid uuid;
	std::memcpy(&uuid, argv[0], uuid.size());
	const char *name = argv[1];
	int noir = std::atoi(argv[2]);
	int nod = std::atoi(argv[3]);
	const char *code = argv[4];
	return parser->Parse(uuid, name, noir, nod, code);
}

int SetNol(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 1);
	Bytecode *bytecode = static_cast<Bytecode *>(data);
	bytecode->nol = std::atoi(argv[0]);
	return 0;
}

typedef std::vector<std::pair<boost::uuids::uuid, int> > NobVector;

}

Bytecode *Bcc(sqlite3 *db)
{
	BlockVector bv;
	{
		BlockParser parser(&bv);
		char *em;
		int e;
		e = sqlite3_exec(db, "SELECT * FROM tacs", Process, &parser, &em);
		if (e != SQLITE_OK) {
			if (e != SQLITE_ABORT)
				std::cerr << "failed to enumerate tacs: " << e << ": " << em << std::endl;
			sqlite3_free(em);
			return nullptr;
		}
	}

	std::unique_ptr<NobVector> nv(new NobVector);
	for (const auto &b : bv) {
		if (nv->empty()) {
			nv->emplace_back(b.uuid, 1);
		} else if (nv->back().first == b.uuid) {
			nv->back().second++;
		} else {
			nv->emplace_back(b.uuid, 1);
		}
	}
	std::unique_ptr<Bytecode> bytecode(new Bytecode);
	// save nol
	{
		char *em;
		int e;
		e = sqlite3_exec(db, "SELECT * FROM nol", SetNol, bytecode.get(), &em);
		if (e != SQLITE_OK) {
			if (e != SQLITE_ABORT)
				std::cerr << "failed to select nol: " << e << ": " << em << std::endl;
			sqlite3_free(em);
			return nullptr;
		}
	}
	// save section headers
	std::unique_ptr<char[]> bu(new char[boost::uuids::uuid::static_size()]);
	for (const auto &nob : *nv) {
		bc::SectionHeader sh;
		const boost::uuids::uuid &uuid(nob.first);
		std::copy(uuid.begin(), uuid.end(), bu.get());
		sh.set_id(bu.get(), uuid.size());
		sh.set_nob(nob.second);
		bytecode->shv->push_back(sh);
	}
	// save block headers
	for (const auto &b : bv) {
		bc::BlockHeader bh;
		bh.set_name(b.name);
		bh.set_noir(b.noir);
		bh.set_nod(b.nod);
		bh.set_noc(b.GetCodeSize());
		bytecode->bhv->push_back(bh);
	}
	// save body
	for (const auto &b : bv)
		bytecode->cv->insert(bytecode->cv->end(),
							 b.code.begin(),
							 b.code.end());

	return bytecode.release();
}

}
}
}
