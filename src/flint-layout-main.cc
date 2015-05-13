/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <vector>

#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_generators.hpp>

#include "lo.pb.h"

#include "bc/pack.h"
#include "db/driver.h"
#include "db/name_loader.h"
#include "db/space_loader.h"

namespace po = boost::program_options;

using std::atoi;
using std::cerr;
using std::endl;
using std::make_pair;
using std::map;
using std::pair;
using std::string;
using std::strlen;
using std::vector;

static const int kUuidSize = 36;

namespace {

typedef map<boost::uuids::uuid, string> ModuleMap;

class ModuleHandler : boost::noncopyable {
public:
	explicit ModuleHandler(ModuleMap *mm) : mm_(mm) {}

	bool Handle(boost::uuids::uuid u, const char *name) {
		mm_->insert(make_pair(u, name));
		return true;
	}

private:
	ModuleMap *mm_;
};

class Name : boost::noncopyable {
public:
	Name(char type, int id, const char *name, const char *unit, double capacity)
		: type_(type),
		  id_(id),
		  name_(name),
		  unit_(unit),
		  capacity_(capacity) {
		assert(unit);
	}

	char type() const {return type_;}
	int id() const {return id_;}
	const string &name() const {return name_;}
	const string &unit() const {return unit_;}
	double capacity() const {return capacity_;}

private:
	char type_;
	int id_;
	string name_;
	string unit_;
	double capacity_;
};

typedef boost::ptr_map<boost::uuids::uuid, boost::ptr_vector<Name> > NameMap;

class NameHandler : boost::noncopyable {
public:
	explicit NameHandler(NameMap *nm)
	: nm_(nm)
	{}

	bool Handle(boost::uuids::uuid u, char type, int id, const char *name, const char *unit, double capacity) {
		(*nm_)[u].push_back(new Name(type, id, name, unit, capacity));
		return true;
	}

private:
	NameMap *nm_;
};

class Node : boost::noncopyable {
public:
	explicit Node(boost::uuids::uuid uuid) : uuid_(uuid), label_() {}
	Node(boost::uuids::uuid uuid, const string &label) : uuid_(uuid), label_(label) {}

	const boost::uuids::uuid &uuid() const {return uuid_;}
	const string &label() const {return label_;}

private:
	boost::uuids::uuid uuid_;
	string label_;
};

typedef boost::ptr_vector<Node> NodeVector;

typedef boost::ptr_map<boost::uuids::uuid, NodeVector> TMap;

class TreeLoader : boost::noncopyable {
public:
	explicit TreeLoader(sqlite3 *db)
		: stmt_(NULL),
		  gen_()
	{
		int e = sqlite3_prepare_v2(db, "SELECT * FROM scopes",
								   -1, &stmt_, NULL);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			exit(EXIT_FAILURE);
		}
	}

	~TreeLoader() {
		sqlite3_finalize(stmt_);
	}

	bool Load(TMap *m) {
		int e;
		for (e = sqlite3_step(stmt_); e == SQLITE_ROW; e = sqlite3_step(stmt_)) {
			const unsigned char *uuid = sqlite3_column_text(stmt_, 0);
			const unsigned char *space_id = sqlite3_column_text(stmt_, 1);
			const unsigned char *label = sqlite3_column_text(stmt_, 2);
			boost::uuids::uuid key = gen_((const char *)space_id);
			if (label) {
				(*m)[key].push_back(new Node(gen_(string((const char *)uuid, kUuidSize)), string((const char *)label)));
			} else {
				(*m)[key].push_back(new Node(gen_(string((const char *)uuid, kUuidSize))));
			}
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt_);
		return true;
	}

private:
	sqlite3_stmt *stmt_;
	boost::uuids::string_generator gen_;
};

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string db_file, output_file;
	int print_help = 0;

	opts.add_options()
		("db", po::value<string>(&db_file), "Input database file")
		("output", po::value<string>(&output_file), "Output file")
		("help,h", "Show this message");
	popts.add("db", 1).add("output", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("db") == 0 ||
					vm.count("output") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " DB OUTPUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	boost::scoped_ptr<db::Driver> driver(new db::Driver(db_file.c_str()));

	boost::scoped_ptr<ModuleMap> mm(new ModuleMap);
	{
		boost::scoped_ptr<db::SpaceLoader> loader(new db::SpaceLoader(driver->db()));
		boost::scoped_ptr<ModuleHandler> handler(new ModuleHandler(mm.get()));
		if (!loader->Load(handler.get())) return EXIT_FAILURE;
	}

	boost::scoped_ptr<NameMap> nm(new NameMap);
	{
		boost::scoped_ptr<db::NameLoader> loader(new db::NameLoader(driver->db()));
		boost::scoped_ptr<NameHandler> handler(new NameHandler(nm.get()));
		if (!loader->Load(handler.get())) return EXIT_FAILURE;
	}

	TMap tm;
	{
		boost::scoped_ptr<TreeLoader> loader(new TreeLoader(driver->db()));
		if (!loader->Load(&tm)) return EXIT_FAILURE;
	}

	std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
	if (!ofs.is_open()) {
		cerr << "could not open output file: " << output_file << endl;
		return EXIT_FAILURE;
	}

	boost::scoped_ptr<lo::Track> track(new lo::Track);
	boost::scoped_ptr<lo::Sector> sector(new lo::Sector);
	boost::scoped_ptr<lo::Data> data(new lo::Data);
	boost::scoped_array<char> tbu(new char[16]); // 16 is UUID's size
	boost::scoped_array<char> sbu(new char[16]); // 16 is UUID's size
	for (TMap::const_iterator it=tm.begin();it!=tm.end();++it) {
		const boost::uuids::uuid &tu(it->first);
		NameMap::const_iterator nmit = nm->find(tu);
		if (nmit == nm->end()) {
			// this is a non-functional
			continue;
		}

		std::copy(tu.begin(), tu.end(), tbu.get());
		track->set_id(tbu.get(), 16);
		track->set_nos(static_cast<int>(it->second->size()));
		track->set_nod(static_cast<int>(nmit->second->size()));

		ModuleMap::const_iterator mmit = mm->find(tu);
		if (mmit == mm->end()) {
			track->clear_name();
		} else {
			track->set_name(mmit->second);
		}

		if (!PackToOstream(*track, &ofs)) {
			return EXIT_FAILURE;
		}

		for (boost::ptr_vector<Name>::const_iterator nit=nmit->second->begin();nit!=nmit->second->end();++nit) {
			data->set_id(nit->id());
			data->set_name(nit->name());
			// TODO
			data->set_size(1);
			switch (nit->type()) {
			case 's': data->set_type(lo::S); break;
			case 't': data->set_type(lo::T); break;
			case 'v': data->set_type(lo::V); break;
			case 'x': data->set_type(lo::X); break;
			default: assert(false); break;
			}
			data->set_unit(nit->unit());
			if (nit->capacity() > 0) data->set_capacity(nit->capacity());
			if (!PackToOstream(*data, &ofs)) {
				return EXIT_FAILURE;
			}
		}

		for (NodeVector::const_iterator sit=it->second->begin();sit!=it->second->end();++sit) {
			std::copy(sit->uuid().begin(), sit->uuid().end(), sbu.get());
			sector->set_id(sbu.get(), 16);
			if (sit->label().empty()) {
				sector->clear_label();
			} else {
				sector->set_label(sit->label());
			}
			if (!PackToOstream(*sector, &ofs)) {
				return EXIT_FAILURE;
			}
		}
	}

	ofs.close();

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
