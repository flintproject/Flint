/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <memory>
#include <string>
#include <set>
#include <boost/program_options.hpp>

#include "bc/binary.h"

#include "isdf/isdf.h"
#include "isdf/reader.h"

namespace po = boost::program_options;

using std::memcpy;
using std::string;

using namespace flint;

namespace {

typedef std::set<int> FieldSet;

class Handler {
public:
	Handler(const Handler &) = delete;
	Handler &operator=(const Handler &) = delete;

	Handler(const FieldSet &fields, std::ostream *os) : fields_(fields), os_(os) {}

	int GetStep(size_t /*buf_size*/, const char *buf) {
		for (int d : fields_) {
			os_->write(buf+sizeof(double)*d, sizeof(double));
			if (!os_->good()) {
				std::cerr << "could not write step: " << d << std::endl;
				return -1;
			}
		}
		return 1;
	}

private:
	const FieldSet &fields_;
	std::ostream *os_;
};

class Dam {
public:
	Dam(const Dam &) = delete;
	Dam &operator=(const Dam &) = delete;

	Dam() : fields_(), reader_() {}

	void AddField(int d) {
		fields_.insert(d);
	}

	bool Pass(std::istream *is, std::ostream *os) {
		if (!reader_.ReadHeader(is)) return false;
		if (!reader_.SkipComment(is)) return false; // discard the original comment
		if (!reader_.ReadDescriptions(is)) return false;
		if (!reader_.ReadUnits(is)) return false;

		isdf::ISDFHeader header = reader_.header();

		std::uint32_t num_objs = 0;
		std::uint32_t num_bytes_descs = 0;
		std::uint32_t num_bytes_units = 0;
		std::unique_ptr<char[]> descriptions(new char[reader_.num_bytes_descs()]);
		std::unique_ptr<char[]> units(new char[reader_.num_bytes_units()]);

		char *d = descriptions.get();
		char *u = units.get();
		const char *p = reader_.descriptions();
		const char *pu = reader_.units();
		std::uint32_t n = 0;
		std::uint32_t len;
		for (int f : fields_) {
			std::uint32_t i = static_cast<std::uint32_t>(f);
			if (i >= header.num_objs) break;

			do {
				memcpy(&len, p, sizeof(len));
				size_t s = sizeof(len)+len;
				if (n == i) {
					memcpy(d, p, s);
					d += s;
					num_bytes_descs += s;
				}
				p += s;

				memcpy(&len, pu, sizeof(len));
				s = sizeof(len)+len;
				if (n == i) {
					memcpy(u, pu, s);
					u += s;
					num_bytes_units += s;
				}
				pu += s;
			} while (n++ < i);

			num_objs++;
		}

		header.num_objs = num_objs;
		header.num_bytes_comment = 0; // discard the original comment
		header.num_bytes_descs = num_bytes_descs;
		header.num_bytes_units = num_bytes_units;
		char h[sizeof(header)];
		memcpy(h, &header, sizeof(header));
		os->write(h, sizeof(header));
		os->write(descriptions.get(), num_bytes_descs);
		os->write(units.get(), num_bytes_units);
		std::unique_ptr<Handler> handler(new Handler(fields_, os));
		return reader_.ReadSteps(*handler, is);
	}

private:
	FieldSet fields_;
	isdf::Reader reader_;
};

} // namespace

int main(int argc, char *argv[])
{
	RequestBinaryStdio();

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string fields;
	string input_file, output_file;
	int print_help = 0;

	opts.add_options()
		("fields,f", po::value<string>(&fields), "Select only these fields")
		("help,h", "Show this message")
		("output,o", po::value<string>(&output_file), "Output file name")
		("input", po::value<string>(&input_file), "Input file name");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help != 0) {
		std::cerr << "usage: isdcut [OPTIONS] [PATH]" << std::endl;
		std::cerr << opts << std::endl;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	std::unique_ptr<Dam> dam(new Dam);
	if (vm.count("fields") > 0) {
		size_t s = fields.size();
		std::unique_ptr<char[]> fa(new char[s+1]());
		memcpy(fa.get(), fields.c_str(), s);
		std::replace(fa.get(), fa.get()+s, ',', '\0');
		char *p = fa.get();
		do {
			int d = strtol(p, &p, 10);
			if (d < 0) {
				std::cerr << "invalid index: " << d << std::endl;
				return EXIT_FAILURE;
			}
			dam->AddField(d);
		} while (p++ < fa.get()+s);
	} else {
		std::cerr << "please specify the --fields option" << std::endl;
		return EXIT_FAILURE;
	}
	if (vm.count("input")) {
		std::ifstream ifs(input_file.c_str(), std::ios::in|std::ios::binary);
		if (!ifs.is_open()) {
			std::cerr << "could not open file: " << input_file << std::endl;
			return EXIT_FAILURE;
		}
		if (vm.count("output")) {
			std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
			if (!ofs.is_open()) {
				std::cerr << "could not open output file: " << output_file << std::endl;
				return EXIT_FAILURE;
			}
			if (!dam->Pass(&ifs, &ofs)) return EXIT_FAILURE;
			ofs.close();
		} else {
			if (!dam->Pass(&ifs, &std::cout)) return EXIT_FAILURE;
		}
		ifs.close();
	} else if (vm.count("output")) {
		std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
		if (!ofs.is_open()) {
			std::cerr << "could not open output file: " << output_file << std::endl;
			return EXIT_FAILURE;
		}
		if (!dam->Pass(&std::cin, &ofs)) return EXIT_FAILURE;
		ofs.close();
	} else {
		if (!dam->Pass(&std::cin, &std::cout)) return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
