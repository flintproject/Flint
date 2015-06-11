/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "load.hh"

#include <cstdio>
#include <cstring>
#include <iostream>

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fprintf;
using std::sprintf;

namespace load {

namespace {

void PrintCommonRecipe(FILE *fp)
{
	fprintf(fp, "param: modeldb\n");
	fprintf(fp, "\tflint-param $< $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "var: modeldb\n");
	fprintf(fp, "\tflint-var $< $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "filter: modeldb spec.txt layout\n");
	fprintf(fp, "\tflint-filter $^ $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "track: filter\n");
	fprintf(fp, "\tflint-track $< $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "isdh: filter\n");
	fprintf(fp, "\tflint-isdh $< $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "form.txt: db\n");
	fprintf(fp, "\tflint-form $< > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "generated-layout: db form.txt\n");
	fprintf(fp, "\tflint-layout $< $@\n"); /* TODO */
	fprintf(fp, "\n");
}

void PrintRecipeForCellml(FILE *fp)
{
	fprintf(fp, "MODEL_LANG = cellml\n");
	fprintf(fp, "\n");
	fprintf(fp, "modeldb: model\n");
	fprintf(fp, "\tflint-concat $< $@\n");
	fprintf(fp, "\tflint-cellml $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "layout: modeldb\n");
	fprintf(fp, "\tflint-layout $< $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "const-bc: modeldb\n");
	fprintf(fp, "\tflint-compile $< input_ivs assign const.db > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "init: modeldb layout const-bc\n");
	fprintf(fp, "\tflint-init $^ $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "load: init param var\n");
	fprintf(fp, "\n");
}

void PrintRecipeForPhml(FILE *fp)
{
	fprintf(fp, "MODEL_LANG = phml\n");
	fprintf(fp, "\n");
	fprintf(fp, "nc: modeldb\n");
	fprintf(fp, "\tflint-nc $< method.txt > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "method.txt: nc\n");
	fprintf(fp, "\n");
	fprintf(fp, "layout: modeldb\n");
	fprintf(fp, "\tflint-layout $< $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "const-bc: modeldb\n");
	fprintf(fp, "\tflint-compile modeldb input_ivs assign const.db > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "after-bc: modeldb\n");
	fprintf(fp, "\tflint-compile modeldb after_eqs event after.db > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "before-bc: modeldb\n");
	fprintf(fp, "\tflint-compile modeldb before_eqs event before.db > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "init: modeldb layout const-bc\n");
	fprintf(fp, "\tflint-init $^ $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "unitoftime: modeldb\n");
	fprintf(fp, "\tflint-unitoftime $< $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "lands.txt: nc unitoftime\n");
	fprintf(fp, "\tflint-lands $^ > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "load: nc init param unitoftime var\n");
	fprintf(fp, "\n");
}

void PrintRecipeForSbml(FILE *fp)
{
	fprintf(fp, "MODEL_LANG = sbml\n");
	fprintf(fp, "\n");
	fprintf(fp, "sbml.txt: model\n");
	fprintf(fp, "\tflint-sbml $< > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "modeldb: model sbml.txt\n");
	fprintf(fp, "\tflint-concat $< $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "layout: modeldb\n");
	fprintf(fp, "\tflint-layout $< $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "output-bc: modeldb\n");
	fprintf(fp, "\tflint-compile $< input_ivs assign assign.db > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "init: modeldb layout output-bc\n");
	fprintf(fp, "\tflint-init $^ $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "load: init param var\n");
	fprintf(fp, "\n");
}

}

bool Config(file::Format format, ConfigMode mode, int dir)
{
	char filename[64]; // big enough
	if (dir) {
		sprintf(filename, "%d/load.mk", dir);
	} else {
		sprintf(filename, "load.mk");
	}
	FILE *fp = fopen(filename, "w");
	if (!fp) {
		std::perror(filename);
		return false;
	}
	switch (format) {
	case file::kIsml:
	case file::kPhml:
		fprintf(fp, "modeldb: model\n");
		fprintf(fp, "\tflint-concat $< $@\n");
		fprintf(fp, "\tflint-phml $@\n");
		fprintf(fp, "\n");
		PrintRecipeForPhml(fp);
		if (mode == kRun) {
			fprintf(fp, "conf.txt: method.txt lands.txt\n");
			fprintf(fp, "\tflint-concat $^ $@\n");
			fprintf(fp, "\n");
		}
		break;
	case file::kPhz:
		fprintf(fp, "modeldb: model\n");
		fprintf(fp, "\tflint-concat $< $@\n");
		fprintf(fp, "\tflint-phz $@ phz\n");
		fprintf(fp, "\tflint-phml $@\n");
		fprintf(fp, "\n");
		PrintRecipeForPhml(fp);
		if (mode == kRun) {
			fprintf(fp, "conf.txt: method.txt lands.txt\n");
			fprintf(fp, "\tflint-concat $^ $@\n");
			fprintf(fp, "\n");
		}
		break;
	case file::kCellml:
		PrintRecipeForCellml(fp);
		if (mode == kRun) {
			fprintf(fp, "conf.txt:\n");
			fprintf(fp, "\techo rk4 2000 0.01 > $@\n");
			fprintf(fp, "\n");
		}
		break;
	case file::kSbml:
		PrintRecipeForSbml(fp);
		if (mode == kRun) {
			fprintf(fp, "conf.txt:\n");
			fprintf(fp, "\techo rk4 100 0.01 > $@\n");
			fprintf(fp, "\n");
		}
		break;
	default:
		cerr << "unexpected file format: " << format << endl;
		return false;
	}
	PrintCommonRecipe(fp);
	if (mode == kRun) {
		/* create the list of all variables */
		fprintf(fp, "spec.txt: modeldb\n");
		fprintf(fp, "\tflint-spec $< > $@\n");
		fprintf(fp, "\n");
	}
	fclose(fp);
	return true;
}

}
