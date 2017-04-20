/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/gnuplot.h"

namespace flint {
namespace gui {

namespace {

void PrintSingleQuoted(const char *filename, FILE *fp)
{
	std::fputc('\'', fp);
	const char *p = filename;
	char c;
	while ( (c = *p++) != '\0') {
		if (c == '\'')
			std::fputs("''", fp);
		else
			std::fputc(c, fp);
	}
	std::fputc('\'', fp);
}

}

bool PlotLineGraph(const LineGraphOption &option, FILE *fp)
{
	if (option.x < 0) // nothing to do
		return false;
	bool y1_empty = option.y1.empty();
	bool y2_empty = option.y2.empty();
	if (y1_empty && y2_empty) // nothing to do
		return false;

	if (y1_empty)
		std::fputs("unset ytics\n", fp);
	else
		std::fputs("set ytics nomirror\n", fp);
	if (y2_empty)
		std::fputs("unset y2tics\n", fp);
	else
		std::fputs("set y2tics nomirror\n", fp);
	std::fputs("plot", fp);
	int n = 0;
	for (auto p : option.y1) {
		auto i = p.first;
		if (n == 0) {
			std::fputc(' ', fp);
			PrintSingleQuoted(option.input_file.c_str(), fp);
		} else {
			std::fputs(", ''", fp);
		}
		std::fprintf(fp,
					 " binary format='%%%udouble' skip=%u using %d:%u axes x1y1 with lines",
					 option.num_variables,
					 option.skip,
					 option.x+1,
					 i+1);
		std::fputs(" title ", fp);
		PrintSingleQuoted(p.second.c_str(), fp);
		++n;
	}
	for (auto p : option.y2) {
		auto i = p.first;
		if (n == 0) {
			std::fputc(' ', fp);
			PrintSingleQuoted(option.input_file.c_str(), fp);
		} else {
			std::fputs(", ''", fp);
		}
		std::fprintf(fp,
					 " binary format='%%%udouble' skip=%u using %d:%u axes x1y2 with lines",
					 option.num_variables,
					 option.skip,
					 option.x+1,
					 i+1);
		std::fputs(" title ", fp);
		PrintSingleQuoted(p.second.c_str(), fp);
		++n;
	}
	std::fputc('\n', fp);
	std::fflush(fp);
	return true;
}

}
}
