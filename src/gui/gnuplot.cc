/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>

#include "gui/gnuplot.h"

namespace flint {
namespace gui {

namespace {

void PrintSingleQuoted(const char *s, std::ostream &os)
{
	os.put('\'');
	const char *p = s;
	char c;
	while ( (c = *p++) != '\0') {
		if (c == '\'')
			os << "''";
		else
			os.put(c);
	}
	os.put('\'');
}

}

bool PlotLineGraph(const LineGraphOption &option, std::ostream &os)
{
	if (option.input_files.empty())
		return false;
	if (option.x < 0)
		return false;
	bool y1_empty = option.y1.empty();
	bool y2_empty = option.y2.empty();
	if (y1_empty && y2_empty)
		return false;

	bool with_id = option.input_files.size() > 1;

	if (!option.log_x)
		os << "un";
	os << "set logscale x" << std::endl;
	if (!y1_empty) {
		if (!option.log_y1)
			os << "un";
		os << "set logscale y" << std::endl;
	}
	if (!y2_empty) {
		if (!option.log_y2)
			os << "un";
		os << "set logscale y2" << std::endl;
	}

	if (y1_empty)
		os << "unset ytics" << std::endl;
	else
		os << "set ytics nomirror" << std::endl;
	if (y2_empty)
		os << "unset y2tics" << std::endl;
	else
		os << "set y2tics nomirror" << std::endl;

	os << "plot";
	for (const auto &ifp : option.input_files) {
		unsigned int id = ifp.first;
		const auto &input_file = ifp.second;
		os.put(' ');
		PrintSingleQuoted(input_file.c_str(), os);
		int n = 0;
		for (auto p : option.y1) {
			auto i = p.first;
			if (n > 0)
				os << ", ''";
			os << " binary format='%" << option.num_variables << "double' skip=" << option.skip
			   << " using " << option.x+1 << ':' << i+1
			   << " axes x1y1 with lines";
			if (option.legend) {
				os << " title ";
				if (with_id) {
					wxString title;
					title << id << ' ' << p.second;
					PrintSingleQuoted(title.c_str(), os);
				} else
					PrintSingleQuoted(p.second.c_str(), os);
			} else {
				os << " notitle";
			}
			++n;
		}
		for (auto p : option.y2) {
			auto i = p.first;
			if (n > 0)
				os << ", ''";
			os << " binary format='%" << option.num_variables << "double' skip=" << option.skip
			   << " using " << option.x+1 << ':' << i+1
			   << " axes x1y2 with lines";
			if (option.legend) {
				os << " title ";
				if (with_id) {
					wxString title;
					title << id << ' ' << p.second;
					PrintSingleQuoted(title.c_str(), os);
				} else
					PrintSingleQuoted(p.second.c_str(), os);
			} else {
				os << " notitle";
			}
			++n;
		}
		os.put(',');
	}
	os << std::endl;
	return true;
}

}
}
