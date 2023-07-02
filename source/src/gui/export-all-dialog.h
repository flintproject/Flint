/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_EXPORT_ALL_DIALOG_H_
#define FLINT_GUI_EXPORT_ALL_DIALOG_H_

#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/progdlg.h>
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class TaskFrame;

class ExportAllDialog : public wxProgressDialog, public wxThreadHelper {
public:
	ExportAllDialog(TaskFrame *frame, const wxString &target_path, int type,
					std::vector<int> &&indice);

	bool Start();

	void OnThreadUpdate(wxThreadEvent &event);

protected:
	virtual wxThread::ExitCode Entry() override;

private:
	void ShowErrorOnExporting(const wxString &message);

	TaskFrame *frame_;
	wxString target_path_;
	int type_;
	std::vector<int> indice_;
};

}
}

#endif
