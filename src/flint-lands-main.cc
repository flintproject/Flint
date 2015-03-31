/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/rational.hpp>
#include <boost/scoped_ptr.hpp>

#include "ipc.pb.h"
#include "phml.pb.h"
#include "bc/pack.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::printf;
using std::string;

namespace {

class UnitOfTimeLoader : boost::noncopyable {
public:
	explicit UnitOfTimeLoader(const char *file) : ifs_(file, std::ios::in|std::ios::binary) {}

	~UnitOfTimeLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		if (!ifs_.is_open()) {
			cerr << "failed to open unitoftime file" << endl;
			return false;
		}
		while (ifs_.peek() != EOF) {
			std::auto_ptr<ipc::TimeUnit> tu(new ipc::TimeUnit);
			if (!UnpackFromIstream(*tu, &ifs_)) {
				cerr << "failed to read TimeUnit" << endl;
				return false;
			}
			handler->AddTimeUnit(tu.release());
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

typedef boost::ptr_map<int, ipc::TimeUnit> TimeUnitMap;

class UnitOfTimeHandler : boost::noncopyable {
public:
	explicit UnitOfTimeHandler(TimeUnitMap *tum) : tum_(tum) {}

	void AddTimeUnit(ipc::TimeUnit *tu) {
		int id = tu->id();
		tum_->insert(id, tu);
	}

private:
	TimeUnitMap *tum_;
};

class NumericalConfigurationLoader : boost::noncopyable {
public:
	explicit NumericalConfigurationLoader(const char *file) : ifs_(file, std::ios::in|std::ios::binary) {}

	~NumericalConfigurationLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	bool Load(phml::NumericalConfiguration *nc) {
		if (!ifs_.is_open()) {
			cerr << "failed to open nc file" << endl;
			return false;
		}
		return nc->ParseFromIstream(&ifs_);
	}

private:
	std::ifstream ifs_;
};

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string nc_file, unitoftime_file;
	int print_help = 0;

	opts.add_options()
		("nc", po::value<string>(&nc_file), "Input numerical-discretization file")
		("unitoftime", po::value<string>(&unitoftime_file), "Input unitoftime file")
		("help,h", "Show this message");
	popts.add("nc", 1).add("unitoftime", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " NC UNITOFTIME" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	phml::NumericalConfiguration nc;
	{
		boost::scoped_ptr<NumericalConfigurationLoader> loader(new NumericalConfigurationLoader(nc_file.c_str()));
		if (!loader->Load(&nc)) return EXIT_FAILURE;
	}

	TimeUnitMap tum;
	{
		boost::scoped_ptr<UnitOfTimeHandler> handler(new UnitOfTimeHandler(&tum));
		boost::scoped_ptr<UnitOfTimeLoader> loader(new UnitOfTimeLoader(unitoftime_file.c_str()));
		if (!loader->Load(handler.get())) return EXIT_FAILURE;
	}

	TimeUnitMap::const_iterator it;
	if (nc.has_sts()) {
		if (nc.has_td()) {
			int sts_unit_id = nc.sts().unit_id();
			it = tum.find(sts_unit_id);
			if (it == tum.end()) {
				cerr << "unknown unit-id of simulation-time-span: " << sts_unit_id << endl;
				return EXIT_FAILURE;
			}
			const ipc::TimeUnit *sts_unit = it->second;
			boost::rational<long> r_sts(sts_unit->n(), sts_unit->d());

			int td_unit_id = nc.td().unit_id();
			it = tum.find(td_unit_id);
			if (it == tum.end()) {
				cerr << "unknown unit-id of time-discretization: " << td_unit_id << endl;
				return EXIT_FAILURE;
			}
			const ipc::TimeUnit *td_unit = it->second;
			boost::rational<long> r_td(td_unit->n(), td_unit->d());

			r_sts /= r_td;

			double len;
			if (std::sscanf(nc.sts().value().c_str(), "%lf", &len) != 1) {
				cerr << "invalid value of simulation-time-span: " << nc.sts().value() << endl;
				return EXIT_FAILURE;
			}
			len *= boost::rational_cast<double>(r_sts);
			printf("%g %s\n", len, nc.td().step().c_str());
		} else {
			printf("%s 0.01\n", nc.sts().value().c_str());
		}
	} else if (nc.has_td()) {
		printf("100 %s\n", nc.td().step().c_str());
	} else {
		printf("100 0.01\n");
	}

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
