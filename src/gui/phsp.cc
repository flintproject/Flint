/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/phsp.h"

#include <cstdio>

#include <wx/wx.h>

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

bool Writer::operator()()
{
	if (!fp_)
		return false;

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
		// TODO
		std::fprintf(fp_, "    </target-set>\n");
		std::fprintf(fp_, "    <parameter-set>\n");
		// TODO: parametrization
		std::fprintf(fp_, "      <parameter name='defaultValue'>\n");
		std::fprintf(fp_, "        <range type='enum'>0</range>\n");
		std::fprintf(fp_, "      </parameter>\n");
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
