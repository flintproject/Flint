/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "tr/translator.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <set>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "bc/index.h"
#include "bc/mounter.h"
#include "cas/dimension.h"
#include "compiler.h"
#include "db/driver.h"
#include "db/statement-driver.h"
#include "file.h"
#include "filter.h"
#include "filter/cutter.h"
#include "filter/writer.h"
#include "flint/background.h"
#include "flint/bc.h"
#include "flint/ct.h"
#include "job.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "load.h"
#include "reduction.h"
#include "run/spec.h"
#include "runtime/flow.h"
#include "system.h"
#include "task.h"
#include "task/config-reader.h"
#include "utf8path.h"

namespace flint {
namespace tr {

Translator::Translator(const Layout *layout, int layer_size, Bytecode *bytecode,
					   std::ostream &os)
	: ct::DataFlowAnalyzer(layout, layer_size, bytecode)
	, os_(os)
{
}

namespace {

void PrintCall1(const bc::Call1 &c1, std::ostream &os)
{
	os << "\ttmp[" << c1.a() << "] = ";
	int a1 = c1.a1();
	switch (c1.op()) {
	case bc::Call1::kAbs:
		os << "fabs(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArccos:
		os << "acos(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArccosh:
		os << "acosh(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArccot:
		os << "atan(1/tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArccoth:
		os << "atanh(1/tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArccsc:
		os << "asin(1/tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArccsch:
		os << "asinh(1/tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArcsec:
		os << "acos(1/tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArcsech:
		os << "acosh(1/tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArcsin:
		os << "asin(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArcsinh:
		os << "asinh(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArctan:
		os << "atan(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kArctanh:
		os << "atanh(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kCeiling:
		os << "ceil(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kCos:
		os << "cos(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kCosh:
		os << "cosh(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kCot:
		os << "cot(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kCoth:
		os << "1/tanh(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kCsc:
		os << "1/sin(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kCsch:
		os << "1/sinh(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kExp:
		os << "exp(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kFactorial:
		os << "assert(0);"; // TODO
		break;
	case bc::Call1::kFloor:
		os << "floor(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kLn:
		os << "log(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kLog10:
		os << "log10(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kMinus1:
		os << "-tmp[" << a1 << "];";
		break;
	case bc::Call1::kRoot1:
		os << "sqrt(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kSec:
		os << "1/cos(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kSech:
		os << "1/cosh(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kSin:
		os << "sin(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kSinh:
		os << "sinh(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kTan:
		os << "tan(tmp[" << a1 << "]);";
		break;
	case bc::Call1::kTanh:
		os << "tanh(tmp[" << a1 << "]);";
		break;
	}
	os << std::endl;
}

void PrintCall2(const bc::Call2 &c2, std::ostream &os)
{
	os << "\ttmp[" << c2.a() << "] = ";
	int a1 = c2.a1();
	int a2 = c2.a2();
	switch (c2.op()) {
	case bc::Call2::kDivide:
		os << "tmp[" << a1 << "] / tmp[" << a2 << "];";
		break;
	case bc::Call2::kEq:
		os << "(tmp[" << a1 << "] == tmp[" << a2 << "]);"; // FIXME
		break;
	case bc::Call2::kGeq:
		os << "(tmp[" << a1 << "] >= tmp[" << a2 << "]);";
		break;
	case bc::Call2::kGt:
		os << "(tmp[" << a1 << "] > tmp[" << a2 << "]);";
		break;
	case bc::Call2::kLeq:
		os << "(tmp[" << a1 << "] <= tmp[" << a2 << "]);";
		break;
	case bc::Call2::kLog:
		os << "log(tmp[" << a2 << "])/log(tmp[" << a1 << "]);";
		break;
	case bc::Call2::kLt:
		os << "(tmp[" << a1 << "] < tmp[" << a2 << "]);";
		break;
	case bc::Call2::kMax:
		os << "max(tmp[" << a1 << "], tmp[" << a2 << "]);";
		break;
	case bc::Call2::kMin:
		os << "min(tmp[" << a1 << "], tmp[" << a2 << "]);";
		break;
	case bc::Call2::kMinus2:
		os << "tmp[" << a1 << "] - tmp[" << a2 << "];";
		break;
	case bc::Call2::kModulo:
		os << "modulo(tmp[" << a1 << "], tmp[" << a2 << "]);";
		break;
	case bc::Call2::kNeq:
		os << "(tmp[" << a1 << "] != tmp[" << a2 << "]);"; // FIXME
		break;
	case bc::Call2::kPlus:
		os << "tmp[" << a1 << "] + tmp[" << a2 << "];";
		break;
	case bc::Call2::kPower:
		os << "pow(tmp[" << a1 << "], tmp[" << a2 << "]);";
		break;
	case bc::Call2::kRemainder:
		os << "fmod(tmp[" << a1 << "], tmp[" << a2 << "]);";
		break;
	case bc::Call2::kRoot2:
		os << "assert(0); /* TODO: kRoot2 */";
		std::cerr << "root with degree is not yet supported" << std::endl;
		break;
	case bc::Call2::kTimes:
		os << "tmp[" << a1 << "] * tmp[" << a2 << "];";
		break;
	}
	os << std::endl;
}

void PrintGen1(const bc::Gen1 &c1, std::ostream &os)
{
	os << "\tassert(0); /* TODO: Gen1 */" << std::endl;
}

void PrintGen2(const bc::Gen2 &c2, std::ostream &os)
{
	os << "\tassert(0); /* TODO: Gen2 */" << std::endl;
}

}

void Translator::PrintHeader(size_t layer_size,
							 double length, double step)
{
	static const unsigned char kHeaderSnippet[] = {
#include "tr/snippet/header.h"
	};
	for (auto c : kHeaderSnippet)
		os_.put(c);

	os_ << std::endl;
	os_ << "static const double length = " << length << ';' << std::endl;
	os_ << "static const double step = " << step << ';' << std::endl;
	os_ << std::endl;
	os_ << "static const int nol = " << GetNol() << ';' << std::endl;
	os_ << "static const size_t layer_size = " << layer_size << ';' << std::endl;
}

void Translator::PrintFunctions()
{
	static const unsigned char kFunctionSnippet[] = {
#include "tr/snippet/function.h"
	};
	os_ << std::endl;
	for (auto c : kFunctionSnippet)
		os_.put(c);

	int si = 0; // section index
	int bi = 0; // block index
	int ci = 0; // code index

	int nos = static_cast<int>(GetShv().size());
	while (si < nos) {
		const bc::SectionHeader &sh(GetShv().at(si++));

		boost::uuids::uuid uuid;
		std::memcpy(&uuid, sh.id().c_str(), uuid.size());
		os_ << std::endl;
		os_ << "/* section " << uuid << " */" << std::endl;

		int bie = bi + sh.nob();
		while (bi < bie) {
			os_ << std::endl;
			os_ << "static void block" << bi << "(int offset)" << std::endl;
			os_ << '{' << std::endl;
			const bc::BlockHeader &bh(GetBhv().at(bi));
			ci = code_offset_[bi++];
			int cib = ci;
			int cie = cib + bh.noc();

			os_ << "\tdouble tmp[" << bh.nod() << "];" << std::endl;
			os_ << std::endl;

			std::set<int> labels;
			while (ci < cie) {
				if (labels.count(ci - cib))
					os_ << "l" << (ci - cib) << ':' << std::endl;
				const bc::Code &code(GetCv().at(ci++));
				switch (code.type()) {
				case bc::Code::kBr:
					{
						os_ << "\tif (tmp[" << code.br().a() << "])" << std::endl;
						os_ << "\t\tgoto l" << code.br().p() << ';' << std::endl;
						labels.insert(code.br().p());
					}
					break;
				case bc::Code::kJmp:
					os_ << "\tgoto l" << code.jmp().p() << ';' << std::endl;
					labels.insert(code.jmp().p());
					break;
				case bc::Code::kCall1:
					PrintCall1(code.call1(), os_);
					break;
				case bc::Code::kCall2:
					PrintCall2(code.call2(), os_);
					break;
				case bc::Code::kLb:
					os_ << "\tassert(0);" << std::endl; // TODO
					break;
				case bc::Code::kLd:
					os_ << "\tassert(0);" << std::endl; // TODO
					break;
				case bc::Code::kGen1:
					PrintGen1(code.gen1(), os_);
					break;
				case bc::Code::kGen2:
					PrintGen2(code.gen2(), os_);
					break;
				case bc::Code::kLoad:
					{
						const bc::Load &load = code.load();
						switch (load.lo()) {
						case -1:
							os_ << "\ttmp[" << load.a() << "] = prev[offset+" << load.so() << "];" << std::endl;
							break;
						case -2:
							os_ << "\ttmp[" << load.a() << "] = data[" << load.so() << "];" << std::endl;
							break;
						default:
							os_ << "\ttmp[" << load.a() << "] = data[offset+" << load.so() << "+(layer_size*" << load.lo() << ")];" << std::endl;
							break;
						}
					}
					break;
				case bc::Code::kLoadi:
					os_ << "\ttmp[" << code.loadi().a() << "] = " << code.loadi().v() << ';' << std::endl;
					break;
				case bc::Code::kRet:
					os_ << "\treturn;" << std::endl;
					break;
				case bc::Code::kStore:
					{
						const bc::Store &store = code.store();
						os_ << "\tdata[offset+" << store.so() << "+(layer_size*" << store.lo() << ")] = tmp[" << store.a() << "];" << std::endl;
					}
					break;
				default:
					os_ << "\tassert(0);" << std::endl; // TODO
					break;
				}
			}
			os_ << '}' << std::endl;
		}
	}
}

void Translator::PrintReductionFunctions()
{
	os_ << std::endl;

	static const unsigned char kReductionSnippet[] = {
#include "tr/snippet/reduction.h"
	};
	for (auto c : kReductionSnippet)
		os_.put(c);

	int i = 0;
	for (const ExecutionUnit &eu : euv_) {
		if (eu.which() == 1) {
			os_ << std::endl;
			const ReductionUnit &ru = boost::get<ReductionUnit>(eu);
			os_ << "static const struct Reduction r" << i++ << " = {" << std::endl;
			switch (ru.reduction()) {
			case Reduction::kUnspecified:
				assert(false);
				break;
			case Reduction::kSum:
				os_ << "\treduce_sum," << std::endl;
				break;
			case Reduction::kMax:
				os_ << "\treduce_max," << std::endl;
				break;
			case Reduction::kMin:
				os_ << "\treduce_min," << std::endl;
				break;
			case Reduction::kMean:
				os_ << "\treduce_mean," << std::endl;
				break;
			case Reduction::kDegree:
				os_ << "\treduce_degree," << std::endl;
				break;
			}
			os_ << "\t" << ru.size() << ',' << std::endl;
			os_ << "\t" << ru.target_addr() << ',' << std::endl;
			size_t nsa = ru.source_addrs().size();
			os_ << "\t" << nsa << ',' << std::endl;
			os_ << "\t{";
			bool first = true;
			for (auto a : ru.source_addrs()) {
				if (first) {
					first = false;
				} else {
					os_ << ", ";
				}
				os_ << a;
			}
			os_ << "}" << std::endl;
			os_ << "};" << std::endl;
		}
	}
}

void Translator::PrintMain(const filter::Writer &writer)
{
	os_ << std::endl;
	writer.PrintCode(&os_);

	os_ << std::endl;
	static const unsigned char kMiscSnippet[] = {
#include "tr/snippet/misc.h"
	};
	for (auto c : kMiscSnippet)
		os_.put(c);

	os_ << std::endl;
	os_ << "int main(int argc, char *argv[])" << std::endl;
	os_ << '{' << std::endl;
	os_ << "\tif (argc == 0)" << std::endl;
	os_ << "\t\treturn EXIT_FAILURE;" << std::endl;
	os_ << "\tif (argc <= 1 || 3 <= argc) {" << std::endl;
	os_ << "\t\tusage(argv[0]);" << std::endl;
	os_ << "\t\treturn EXIT_FAILURE;" << std::endl;
	os_ << "\t}" << std::endl;
	os_ << "\tdata = calloc(nol * layer_size, sizeof(double));" << std::endl;
	os_ << "\tif (!data) {" << std::endl;
	os_ << "\t\tfprintf(stderr, \"calloc failed\\n\");" << std::endl;
	os_ << "\t\treturn EXIT_FAILURE;" << std::endl;
	os_ << "\t}" << std::endl;
	os_ << "\tprev = calloc(nol * layer_size, sizeof(double));" << std::endl;
	os_ << "\tif (!prev) {" << std::endl;
	os_ << "\t\tfprintf(stderr, \"calloc failed\\n\");" << std::endl;
	os_ << "\t\treturn EXIT_FAILURE;" << std::endl;
	os_ << "\t}" << std::endl;
	os_ << "\tfill_input();" << std::endl;
	os_ << "\tdata[" << kIndexEnd << "] = prev[" << kIndexEnd << "] = length;" << std::endl;
	os_ << "\tdata[" << kIndexDt << "] = prev[" << kIndexDt << "] = step;" << std::endl;
	os_ << "\tFILE *fp = fopen(argv[1], \"wb\");" << std::endl;
	os_ << "\tif (!fp) {" << std::endl;
	os_ << "\t\tperror(argv[1]);" << std::endl;
	os_ << "\t\treturn EXIT_FAILURE;" << std::endl;
	os_ << "\t}" << std::endl;
	os_ << "\tif (!fwrite(isdh, isdh_length, 1, fp))" << std::endl;
	os_ << "\t\treturn EXIT_FAILURE;" << std::endl;
	os_ << "\tif (!write_output(fp))" << std::endl;
	os_ << "\t\treturn EXIT_FAILURE;" << std::endl;
	os_ << "\tdo {" << std::endl;

	int i = 0;
	for (const ExecutionUnit &eu : euv_) {
		if (eu.which() == 0) {
			const CalculationUnit &cu = boost::get<CalculationUnit>(eu);
			int bi = cu.block_index();
			int offset = cu.offset();
			os_ << "\t\tblock" << bi << "("
				<< offset
				<< ");"
				<< std::endl;
		} else {
			os_ << "\t\tr" << i << ".reduce(&r" << i << ");" << std::endl;
			++i;
		}
	}

	os_ << "\t\tdata[" << kIndexTime << "] += prev[" << kIndexDt << "];" << std::endl;
	os_ << "\t\tif (!write_output(fp))" << std::endl;
	os_ << "\t\t\treturn EXIT_FAILURE;" << std::endl;
	os_ << "\t\tdouble last_time = data[" << kIndexTime << "];" << std::endl;
	os_ << "\t\tdouble *buf = data;" << std::endl;
	os_ << "\t\tdata = prev;" << std::endl;
	os_ << "\t\tprev = buf;" << std::endl;
	os_ << "\t\tdata[" << kIndexTime << "] = last_time;" << std::endl;
	os_ << "\t} while (data["
		<< kIndexTime
		<< "] < data["
		<< kIndexEnd
		<< "]);" << std::endl;
	os_ << "\tfclose(fp);" << std::endl;
	os_ << "\tfree(data);" << std::endl;
	os_ << "\tfree(prev);" << std::endl;
	os_ << "\treturn EXIT_SUCCESS;" << std::endl;
	os_ << '}' << std::endl;
}

}
}
