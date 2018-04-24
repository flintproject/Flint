/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/script-frame.h"

#include "gui/view-frame.h"

namespace flint {
namespace gui {

ScriptFrame::ScriptFrame(ViewFrame *parent)
	: wxFrame(parent, wxID_ANY, "Gnuplot script")
	, script_text_(new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize,
								  wxTE_MULTILINE|wxTE_READONLY))
{
	auto box = new wxBoxSizer(wxVERTICAL);
	box->Add(script_text_, 1 /* vertically stretchable */, wxEXPAND /* horizontally stretchable */);
	script_text_->SetMinSize(wxSize(600, 200));
	SetSizerAndFit(box);

	Bind(wxEVT_CLOSE_WINDOW, &ScriptFrame::OnClose, this);
}

ScriptFrame::~ScriptFrame() = default;

void ScriptFrame::Clear()
{
	script_text_->Clear();
}

wxTextCtrl *ScriptFrame::GetTextCtrl()
{
	return script_text_;
}

void ScriptFrame::OnClose(wxCloseEvent &)
{
	Show(false);
	wxStaticCast(GetParent(), ViewFrame)->UncheckShowScript();
}

}
}
