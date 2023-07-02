/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/sedml.h"

#include <cstdio>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

#include "gui/configuration.h"
#include "gui/document.h"
#include "gui/filename.h"
#include "gui/simulation.h"

namespace flint {
namespace gui {

namespace {

class Writer {
public:
	Writer(const Simulation *sim, const wxString &file);

	~Writer();

	bool operator()();

private:
	const Simulation *sim_;
	FILE *fp_;
};

Writer::Writer(const Simulation *sim, const wxString &file)
	: sim_(sim)
	, fp_(wxFopen(file, "wb"))
{
	if (!fp_)
		wxLogError("failed to open %s", file);
}

Writer::~Writer()
{
	if (fp_)
		std::fclose(fp_);
}

bool Writer::operator()()
{
	if (!fp_)
		return false;

	std::fprintf(fp_, "<?xml version='1.0' encoding='UTF-8'?>\n");
	std::fprintf(fp_, "<sedML xmlns='http://sed-ml.org/' xmlns:m='http://www.w3.org/1998/Math/MathML' xmlns:flint='http://physiodesigner.org/namespace/flint' version='1' level='1'>\n");
	std::fprintf(fp_, "  <listOfSimulations>\n");
	int i = 0;
	for (const auto &p : sim_->entries) {
		const auto *doc = p.first;
		const auto *config = p.second;
		std::fprintf(fp_, "    <uniformTimeCourse id='sim%d' name='Simulation %d' initialTime='0' outputStartTime='%f' outputEndTime='%f' numberOfPoints='%d' flint:granularity='%d'",
					 i,
					 i,
					 config->GetOutputStartTime(doc),
					 config->GetOutputEndTime(doc),
					 config->GetNumberOfPoints(doc),
					 config->granularity);
		if (!config->dps_path.empty()) {
			const auto &dps_path_u = config->dps_path.utf8_str();
			std::fprintf(fp_, " flint:dpsPath='%s'", dps_path_u.data());
		}
		std::fputs(">\n", fp_);
		std::fprintf(fp_, "      <algorithm kisaoID='KISAO:%s'/>\n",
					 config->GetKisaoId());
		std::fprintf(fp_, "    </uniformTimeCourse>\n");
		++i;
	}
	std::fprintf(fp_, "  </listOfSimulations>\n");
	std::fprintf(fp_, "  <listOfModels>\n");
	i = 0;
	for (const auto &p : sim_->entries) {
		const auto *doc = p.first;
		const auto &path = doc->path();
		auto path_u = path.utf8_str();
		std::fprintf(fp_,
					 "    <model id='model%d' name='Model %d' language='urn:sedml:language:%s' source='%s'/>\n",
					 i, i, doc->GetFormat(), path_u.data());
		++i;
	}
	std::fprintf(fp_, "  </listOfModels>\n");
	std::fprintf(fp_, "  <listOfTasks>\n");
	i = 0;
	for (const auto &p : sim_->entries) {
		(void)p;
		std::fprintf(fp_,
					 "    <task id='task%d' name='Task %d' modelReference='model%d' simulationReference='sim%d'/>\n",
					 i, i, i, i);
		++i;
	}
	std::fprintf(fp_, "  </listOfTasks>\n");
	std::fprintf(fp_, "  <listOfDataGenerators>\n");
	i = 0;
	int j = 0;
	for (const auto &p : sim_->entries) {
		const auto *doc = p.first;
		const auto *config = p.second;
		std::vector<std::string> v;
		config->GetOutputVariables(doc, &v);
		for (const auto &name : v) {
			std::fprintf(fp_, "    <dataGenerator id='dg%d' name='%s'>\n",
						 j, name.c_str());
			std::fprintf(fp_, "      <listOfVariables>\n");
			std::fprintf(fp_, "        <variable id='v%d' taskReference='task%d' target='%s'/>\n",
						 j, i, name.c_str());
			std::fprintf(fp_, "      </listOfVariables>\n");
			std::fprintf(fp_, "      <m:math>\n");
			std::fprintf(fp_, "        <m:ci>v%d</m:ci>\n", j);
			std::fprintf(fp_, "      </m:math>\n");
			std::fprintf(fp_, "    </dataGenerator>\n");
			++j;
		}
		++i;
	}
	std::fprintf(fp_, "  </listOfDataGenerators>\n");
	std::fprintf(fp_, "</sedML>\n");
	return true;
}

}

bool WriteSedml(const Simulation *sim, const wxFileName &filename)
{
	Writer writer(sim, filename.GetFullPath());
	return writer();
}

}
}
