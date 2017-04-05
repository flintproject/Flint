/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/phsp-editor-dialog.h"

#include <cassert>
#include <cstdint>
#include <unordered_map>
#include <unordered_set>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/propgrid/propgrid.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class ParamTreeNode {
public:
	virtual wxString GetType() const = 0;
	virtual wxString GetName() const = 0;
	virtual wxString GetValues() const = 0;
};

class ParamTreeParameter : public ParamTreeNode {
public:
	virtual wxString GetType() const override {return "parameter";}
	virtual wxString GetName() const override {return name_;}
	virtual wxString GetValues() const override;

	void set_name(const wxString &name) {name_ = name;}

private:
	wxString name_;
};

wxString ParamTreeParameter::GetValues() const
{
	return ""; // FIXME
}

class ParamTreeProduct : public ParamTreeNode {
public:
	virtual wxString GetType() const override {return "product";}
	virtual wxString GetName() const override {return "";}
	virtual wxString GetValues() const override {return "";}
};

class ParamTreeZip : public ParamTreeNode {
public:
	virtual wxString GetType() const override {return "zip";}
	virtual wxString GetName() const override {return "";}
	virtual wxString GetValues() const override {return "";}
};

/*
 * This is the data model to be displayed by wxDataViewCtrl.
 */
class ParamTreeViewModel : public wxDataViewModel {
public:
	virtual unsigned int GetChildren(const wxDataViewItem &item, wxDataViewItemArray &children) const override;
	virtual unsigned int GetColumnCount() const override;
	virtual wxString GetColumnType(unsigned int col) const override;
	virtual wxDataViewItem GetParent(const wxDataViewItem &item) const override;
	virtual void GetValue(wxVariant &variant, const wxDataViewItem &item, unsigned int col) const override;
	virtual bool IsContainer(const wxDataViewItem &item) const override;
	virtual bool SetValue(const wxVariant &variant, const wxDataViewItem &item, unsigned int col) override;

	wxString GetItemText(const wxDataViewItem &item) const;

	wxDataViewItem AddParameter(const wxDataViewItem &parent_item);
	wxDataViewItem AddProduct(const wxDataViewItem &parent_item);
	wxDataViewItem AddZip(const wxDataViewItem &parent_item);

	void DeleteItem(const wxDataViewItem &item);

private:
	std::unordered_set<std::unique_ptr<ParamTreeNode> > nodes_;
	std::unordered_map<std::uintptr_t, std::uintptr_t> pm_; // parent map: id to parent id
};

unsigned int ParamTreeViewModel::GetChildren(const wxDataViewItem &item, wxDataViewItemArray &children) const
{
	auto id = reinterpret_cast<std::uintptr_t>(item.GetID());
	for (auto p : pm_)
		if (id == p.second)
			children.Add(wxDataViewItem(reinterpret_cast<void *>(p.first)));
	return static_cast<unsigned int>(children.GetCount());
}

unsigned int ParamTreeViewModel::GetColumnCount() const
{
	return 3u;
}

wxString ParamTreeViewModel::GetColumnType(unsigned int /*col*/) const
{
	return "string";
}

wxDataViewItem ParamTreeViewModel::GetParent(const wxDataViewItem &item) const
{
	auto id = reinterpret_cast<std::uintptr_t>(item.GetID());
	assert(id != 0);
	auto pit = pm_.find(id);
	if (pit == pm_.end())
		return wxDataViewItem(nullptr);
	return wxDataViewItem(reinterpret_cast<void *>(pit->second));
}

void ParamTreeViewModel::GetValue(wxVariant &variant, const wxDataViewItem &item, unsigned int col) const
{
	ParamTreeNode *node = reinterpret_cast<ParamTreeNode *>(item.GetID());
	if (!node)
		return;
	switch (col) {
	case 0:
		variant = node->GetType();
		break;
	case 1:
		variant = node->GetName();
		break;
	default:
		variant = node->GetValues();
		break;
	}
}

bool ParamTreeViewModel::IsContainer(const wxDataViewItem &item) const
{
	auto *node = reinterpret_cast<ParamTreeNode *>(item.GetID());
	if (!node)
		return true;
	return !dynamic_cast<ParamTreeParameter *>(node);
}

bool ParamTreeViewModel::SetValue(const wxVariant &variant, const wxDataViewItem &item, unsigned int col)
{
	if (col != 1)
		return false;
	auto *node = reinterpret_cast<ParamTreeNode *>(item.GetID());
	if (!node)
		return false;
	auto *parameter = dynamic_cast<ParamTreeParameter *>(node);
	if (!parameter)
		return false;
	parameter->set_name(variant.GetString());
	return true;
}

wxString ParamTreeViewModel::GetItemText(const wxDataViewItem &item) const
{
	auto *node = reinterpret_cast<ParamTreeNode *>(item.GetID());
	assert(node);
	auto *parameter = dynamic_cast<ParamTreeParameter *>(node);
	if (parameter)
		return parameter->GetName();
	return node->GetType();
}

wxDataViewItem ParamTreeViewModel::AddParameter(const wxDataViewItem &parent_item)
{
	auto p = nodes_.emplace(new ParamTreeParameter);
	assert(p.second);
	auto *node = p.first->get();
	pm_.emplace(reinterpret_cast<std::uintptr_t>(node),
				reinterpret_cast<std::uintptr_t>(parent_item.GetID()));
	auto item = wxDataViewItem(node);
	ItemAdded(parent_item, item); // to inform that the item is added
	return item;
}

wxDataViewItem ParamTreeViewModel::AddProduct(const wxDataViewItem &parent_item)
{
	auto p = nodes_.emplace(new ParamTreeProduct);
	assert(p.second);
	auto *node = p.first->get();
	pm_.emplace(reinterpret_cast<std::uintptr_t>(node),
				reinterpret_cast<std::uintptr_t>(parent_item.GetID()));
	auto item = wxDataViewItem(node);
	ItemAdded(parent_item, item); // to inform that the item is added
	return item;
}

wxDataViewItem ParamTreeViewModel::AddZip(const wxDataViewItem &parent_item)
{
	auto p = nodes_.emplace(new ParamTreeZip);
	assert(p.second);
	auto *node = p.first->get();
	pm_.emplace(reinterpret_cast<std::uintptr_t>(node),
				reinterpret_cast<std::uintptr_t>(parent_item.GetID()));
	auto item = wxDataViewItem(node);
	ItemAdded(parent_item, item); // to inform that the item is added
	return item;
}

void ParamTreeViewModel::DeleteItem(const wxDataViewItem &item)
{
	auto *node = reinterpret_cast<ParamTreeNode *>(item.GetID());
	assert(node);
	auto pit = pm_.find(reinterpret_cast<std::uintptr_t>(node));
	assert(pit != pm_.end());
	auto parent = wxDataViewItem(reinterpret_cast<void *>(pit->second));
	ItemDeleted(parent, item); // to inform that the item is deleted
	pm_.erase(pit);
	for (auto nit=nodes_.begin();nit!=nodes_.end();++nit) {
		if (nit->get() == node) {
			nodes_.erase(nit);
			return;
		}
	}
}


class ParamIntervalCtrl : public wxPropertyGrid {
public:
	explicit ParamIntervalCtrl(wxWindow *parent);
private:
};

ParamIntervalCtrl::ParamIntervalCtrl(wxWindow *parent)
	: wxPropertyGrid(parent)
{
	Append(new wxFloatProperty("lower", wxPG_LABEL, 0.0));
	Append(new wxFloatProperty("upper", wxPG_LABEL, 10.0));
	Append(new wxFloatProperty("step", wxPG_LABEL, 1.0));
	SetMinSize(wxSize(300, 300));
	SetSplitterLeft();
}

class ParamEnumCtrl : public wxTextCtrl {
public:
	explicit ParamEnumCtrl(wxWindow *parent);
private:
};

ParamEnumCtrl::ParamEnumCtrl(wxWindow *parent)
	: wxTextCtrl(parent, wxID_ANY, "0,0.1,0.2", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE)
{
	SetMinSize(wxSize(300, 300));
}

class ParamGaussianCtrl : public wxPropertyGrid {
public:
	explicit ParamGaussianCtrl(wxWindow *parent);

private:
};

ParamGaussianCtrl::ParamGaussianCtrl(wxWindow *parent)
	: wxPropertyGrid(parent)
{
	Append(new wxFloatProperty("mean", wxPG_LABEL, 0));
	Append(new wxFloatProperty("standard deviation", wxPG_LABEL, 1));
	Append(new wxFloatProperty("count", wxPG_LABEL, 10));
	SetMinSize(wxSize(300, 300));
	SetSplitterLeft();
}

class ParamUniformCtrl : public wxPropertyGrid {
public:
	explicit ParamUniformCtrl(wxWindow *parent);

private:
};

ParamUniformCtrl::ParamUniformCtrl(wxWindow *parent)
	: wxPropertyGrid(parent)
{
	Append(new wxFloatProperty("minimum", wxPG_LABEL, 0));
	Append(new wxFloatProperty("maximum", wxPG_LABEL, 1));
	Append(new wxFloatProperty("count", wxPG_LABEL, 10));
	SetMinSize(wxSize(300, 300));
	SetSplitterLeft();
}

enum {
	kIdParamTreeViewCtrl = wxID_HIGHEST + 1
};

PhspEditorDialog::PhspEditorDialog(wxWindow *parent)
	: wxDialog(parent, wxID_ANY, "Edit parameter set")
	, model_(new ParamTreeViewModel)
	, tree_view_(new wxDataViewCtrl(this, kIdParamTreeViewCtrl))
	, book_(new wxChoicebook(this, wxID_ANY))
{
	tree_view_->AssociateModel(model_);
	model_->DecRef(); // avoid memory leak, see wxDataViewModel's documentation

	tree_view_->AppendTextColumn("Type", 0, wxDATAVIEW_CELL_INERT);
	tree_view_->AppendTextColumn("Name", 1, wxDATAVIEW_CELL_EDITABLE);
	tree_view_->AppendTextColumn("Values", 2, wxDATAVIEW_CELL_INERT);
	tree_view_->Fit();

	auto button_apply = new wxButton(this, wxID_ANY, "Apply");
	button_apply->Bind(wxEVT_BUTTON, &PhspEditorDialog::OnApply, this);

	auto button_plus = new wxButton(this, wxID_ANY, "+");
	button_plus->Bind(wxEVT_BUTTON, &PhspEditorDialog::OnPlus, this);
	auto button_minus = new wxButton(this, wxID_ANY, "-");
	button_minus->Bind(wxEVT_BUTTON, &PhspEditorDialog::OnMinus, this);

	book_->AddPage(new wxPanel(book_), "-");
	book_->AddPage(new ParamEnumCtrl(book_), "range - enum");
	book_->AddPage(new ParamIntervalCtrl(book_), "range - interval");
	book_->AddPage(new ParamGaussianCtrl(book_), "random - Gaussian");
	book_->AddPage(new ParamUniformCtrl(book_), "random - uniform");
	book_->Disable();

	// sizer
	auto pm_sizer = new wxBoxSizer(wxHORIZONTAL);
	pm_sizer->Add(button_plus);
	pm_sizer->Add(button_minus);
	auto left_sizer = new wxBoxSizer(wxVERTICAL);
	left_sizer->Add(tree_view_, 1 /* vertically stretchable */, wxEXPAND);
	left_sizer->Add(pm_sizer);
	auto upper_sizer = new wxBoxSizer(wxHORIZONTAL);
	upper_sizer->Add(left_sizer, 1 /* horizontally stretchable */, wxEXPAND);
	upper_sizer->Add(book_, 1 /* horizontally stretchable */, wxEXPAND);
	auto lower_sizer = new wxBoxSizer(wxHORIZONTAL);
	lower_sizer->Add(new wxButton(this, wxID_OK, "OK"));
	lower_sizer->Add(button_apply);
	lower_sizer->Add(new wxButton(this, wxID_CANCEL, "Cancel"));
	auto top_sizer = new wxBoxSizer(wxVERTICAL);
	top_sizer->Add(upper_sizer, 1 /* vertically stretchable */, wxEXPAND);
	top_sizer->Add(lower_sizer, 0, wxALIGN_RIGHT /* preserving its original size */);
	SetSizerAndFit(top_sizer);
}

wxBEGIN_EVENT_TABLE(PhspEditorDialog, wxDialog)
EVT_DATAVIEW_SELECTION_CHANGED(kIdParamTreeViewCtrl, PhspEditorDialog::OnSelectionChanged)
wxEND_EVENT_TABLE()

void PhspEditorDialog::OnApply(wxCommandEvent &)
{
	// TODO
}

void PhspEditorDialog::OnPlus(wxCommandEvent &)
{
	auto item = tree_view_->GetCurrentItem();
	if (item.IsOk() && !model_->IsContainer(item))
		return; // give up

	wxArrayString array;
	array.Add("parameter");
	array.Add("product");
	array.Add("zip");
	int r = wxGetSingleChoiceIndex("Choose a type of item to be added:", "Adding an item", array, this);
	if (r == -1) // cancelled
		return;
	if (r > 0) // product or zip
		item = (r == 1) ? model_->AddProduct(item) : model_->AddZip(item);
	item = model_->AddParameter(item);
	tree_view_->Select(item);
}

void PhspEditorDialog::OnMinus(wxCommandEvent &)
{
	auto item = tree_view_->GetCurrentItem();
	if (item.IsOk()) {
		auto message = wxString::Format("Are you sure to delete %s?", model_->GetItemText(item));
		wxMessageDialog dialog(this, message, "Delete it?", wxYES_NO|wxNO_DEFAULT);
		if (dialog.ShowModal() == wxID_YES)
			model_->DeleteItem(item);
	}
}

void PhspEditorDialog::OnSelectionChanged(wxDataViewEvent &event)
{
	auto item = event.GetItem();
	auto *node = reinterpret_cast<ParamTreeNode *>(item.GetID());
	if (dynamic_cast<ParamTreeParameter *>(node)) {
		book_->Enable();
		book_->ChangeSelection(1);
	} else {
		book_->ChangeSelection(0);
		book_->Disable();
	}
}

}
}
