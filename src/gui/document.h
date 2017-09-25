/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_DOCUMENT_H_
#define FLINT_GUI_DOCUMENT_H_

#include <unordered_map>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

#include "file.h"
#include "gui/configuration.h"
#include "lo.pb.h"
#include "sqlite3.h"

namespace flint {
namespace gui {

class Document
{
public:
	Document(const boost::filesystem::path &dir, const wxString &path, std::vector<double> data);

	const wxString &path() const {return path_;}
	file::Format format() const {return format_;}

	const Configuration &initial_config() const {return initial_config_;}

	const wxArrayString &choices_method() const {return choices_method_;}
	const wxArrayString &choices_time() const {return choices_time_;}

	const std::vector<lo::Column> &param() const {return param_;}
	const std::vector<lo::Column> &var() const {return var_;}

	// For the attribute value of SED-ML and PHSP
	const char *GetFormat() const;

	double GetData(int i) const {return data_[i];}

	int GetDenominator(int i) const {return denominators_time_[i];}
	int GetNumerator(int i) const {return numerators_time_[i];}

	bool Load();

private:
	bool LoadFileFormat();
	bool LoadAlgorithm();
	bool LoadUnitOfTime(sqlite3 *db);
	bool LoadNc();
	bool LoadParam();
	bool LoadVar();

	boost::filesystem::path dir_;
	const wxString path_;
	file::Format format_;
	std::vector<double> data_;

	wxArrayString choices_method_;
	wxArrayString choices_time_; // length, step, and start
	std::vector<long> denominators_time_;
	std::vector<long> numerators_time_;
	std::unordered_map<int, int> ids_time_; // id to index

	Configuration initial_config_;

	std::vector<lo::Column> param_;
	std::vector<lo::Column> var_;
};

}
}

#endif
