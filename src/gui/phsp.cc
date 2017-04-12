/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/phsp.h"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>

#include <boost/uuid/uuid_io.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

#include "gui/configuration.h"
#include "gui/document.h"
#include "gui/formula.h"
#include "gui/simulation.h"

namespace flint {
namespace gui {

namespace {

class Writer {
public:
	Writer(const Simulation *sim, const char *file);

	~Writer();

	bool operator()();

private:
	const Simulation *sim_;
	FILE *fp_;
};

Writer::Writer(const Simulation *sim, const char *file)
	: sim_(sim)
	, fp_(std::fopen(file, "wb"))
{
	if (!fp_)
		wxLogError("failed to open %s", file);
}

Writer::~Writer()
{
	if (fp_)
		std::fclose(fp_);
}

void PrintLnWithIndent(const char *s, int indent, FILE *fp)
{
	for (int i=0;i<indent;i++)
		std::fputc(' ', fp);
	std::fputs(s, fp);
	std::fputc('\n', fp);
}

void WriteSubtreeOfParameterSet(const ParamMap &param_map,
								std::uintptr_t n,
								int indent,
								FILE *fp)
{
	auto *node = reinterpret_cast<ParamTreeNode *>(n);
	assert(node);
	const auto *parameter = dynamic_cast<const ParamTreeParameter *>(node);
	if (parameter) {
		auto xml = parameter->GetXml(indent);
		std::fputs(xml.ToAscii(), fp); // TODO: UTF-8?
	} else if (dynamic_cast<ParamTreeProduct *>(node)) {
		PrintLnWithIndent("<product>", indent, fp);
		auto p = param_map.equal_range(n);
		for (auto it=p.first;it!=p.second;++it)
			WriteSubtreeOfParameterSet(param_map, it->second, indent+2, fp);
		PrintLnWithIndent("</product>", indent, fp);
	} else if (dynamic_cast<ParamTreeZip *>(node)) {
		PrintLnWithIndent("<zip>", indent, fp);
		auto p = param_map.equal_range(n);
		for (auto it=p.first;it!=p.second;++it)
			WriteSubtreeOfParameterSet(param_map, it->second, indent+2, fp);
		PrintLnWithIndent("</zip>", indent, fp);
	} else {
		assert(false);
	}
}

void WriteParameterSet(const ParamMap &param_map, FILE *fp)
{
	if (param_map.empty()) {
		std::fprintf(fp, "      <parameter name='defaultValue'>\n");
		std::fprintf(fp, "        <range type='enum'>0</range>\n");
		std::fprintf(fp, "      </parameter>\n");
		return;
	}
	auto p = param_map.equal_range(0);
	assert(p.first != param_map.end());
	for (auto it=p.first;it!=p.second;++it)
		WriteSubtreeOfParameterSet(param_map, it->second, 6, fp);
}

bool Writer::operator()()
{
	if (!fp_)
		return false;

	std::ostringstream oss, es;
	std::fprintf(fp_, "<?xml version='1.0' encoding='UTF-8'?>\n");
	std::fprintf(fp_, "<phsp xmlns='http://www.physiodesigner.org/2013/ns/phsp/1.0' xmlns:m='http://www.w3.org/1998/Math/MathML' version='1.0'>\n");
	for (const auto &p : sim_->entries) {
		const auto *doc = p.first;
		const auto *config = p.second;
		const auto &path = doc->path();
		auto path_u = path.utf8_str();
		std::fprintf(fp_, "  <model format='%s' iref='%s'>\n",
					 doc->GetFormat(), path_u.data());
		std::fprintf(fp_, "    <target-set>\n");
		for (const auto &target : config->GetTargets(doc)) {
			const auto &column = target.first;
			const auto &value = target.second;
			std::unique_ptr<Formula> formula(Formula::FromUtf8(value.c_str(), es));
			if (!formula) {
				wxLogError("failed to parse parameter value: %s", es.str());
				return false;
			}
			// TODO: for SBML
			boost::uuids::uuid uuid;
			std::memcpy(&uuid, column.uuid().c_str(), 16);
			auto u = boost::uuids::to_string(uuid);
			std::fprintf(fp_, "      <target module-id='%s' physical-quantity-id='%d'>\n",
						 u.c_str(), column.id());
			formula->WriteMathML("m", oss);
			std::fprintf(fp_, "        <value><m:math>%s</m:math></value>\n",
						 oss.str().c_str());
			oss.str("");
			std::fprintf(fp_, "      </target>\n");
		}
		std::fprintf(fp_, "    </target-set>\n");
		std::fprintf(fp_, "    <parameter-set>\n");
		WriteParameterSet(config->param_map, fp_);
		std::fprintf(fp_, "    </parameter-set>\n");
		std::fprintf(fp_, "  </model>\n");
	}
	std::fprintf(fp_, "</phsp>\n");
	return true;
}

}

bool WritePhsp(const Simulation *sim, const wxFileName &filename)
{
	auto path = filename.GetFullPath();
	Writer writer(sim, path.c_str()); // TODO: check locale-dependency
	return writer();
}

}
}
