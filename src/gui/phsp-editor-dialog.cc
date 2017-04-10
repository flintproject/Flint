/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/phsp-editor-dialog.h"

#include <cassert>
#include <cstdint>
#include <memory>
#include <unordered_map>
#include <unordered_set>

#include "gui/param-tree.h"

namespace flint {
namespace gui {

namespace {

enum {
	kPageNA,
	kPageInterval,
	kPageEnum,
	kPageGaussian,
	kPageUniform
};

class ParamValues {
public:
	unsigned int page() const {return page_;}

	virtual ParamValues *Copy() const = 0;
	virtual wxString GetText() const = 0;

protected:
	explicit ParamValues(size_t page);

private:
	unsigned int page_;
};

ParamValues::ParamValues(size_t page)
	: page_(page)
{}

class ParamInterval : public ParamValues {
public:
	ParamInterval(double lower = 0,
				  double upper = 4,
				  double step = 1);

	virtual ~ParamInterval() = default;

	virtual ParamValues *Copy() const override;
	virtual wxString GetText() const override;

	double lower() const {return lower_;}
	double upper() const {return upper_;}
	double step() const {return step_;}

private:
	double lower_;
	double upper_;
	double step_;
};

ParamInterval::ParamInterval(double lower, double upper, double step)
	: ParamValues(kPageInterval)
	, lower_(lower)
	, upper_(upper)
	, step_(step)
{}

ParamValues *ParamInterval::Copy() const
{
	return new ParamInterval(*this);
}

wxString ParamInterval::GetText() const
{
	return wxString::Format("[interval] lower: %g, upper: %g, step: %g", lower_, upper_, step_);
}

class ParamEnum : public ParamValues {
public:
	explicit ParamEnum(const wxString &body);

	virtual ~ParamEnum() = default;

	virtual ParamValues *Copy() const override;
	virtual wxString GetText() const override;

	const wxString &body() const {return body_;}

private:
	wxString body_;
};

ParamEnum::ParamEnum(const wxString &body)
	: ParamValues(kPageEnum)
	, body_(body)
{}

ParamValues *ParamEnum::Copy() const
{
	return new ParamEnum(*this);
}

wxString ParamEnum::GetText() const
{
	return wxString::Format("[enum] %s", body_);
}

class ParamGaussian : public ParamValues {
public:
	ParamGaussian(double mean = 0, double stddev = 1, int count = 5);

	virtual ~ParamGaussian() = default;

	virtual ParamValues *Copy() const override;
	virtual wxString GetText() const override;

	double mean() const {return mean_;}
	double stddev() const {return stddev_;}
	int count() const {return count_;}

private:
	double mean_;
	double stddev_;
	int count_;
};

ParamGaussian::ParamGaussian(double mean, double stddev, int count)
	: ParamValues(kPageGaussian)
	, mean_(mean)
	, stddev_(stddev)
	, count_(count)
{}

ParamValues *ParamGaussian::Copy() const
{
	return new ParamGaussian(*this);
}

wxString ParamGaussian::GetText() const
{
	return wxString::Format("[Gaussian] mean: %g, stddev: %g, count: %d",
							mean_, stddev_, count_);
}

class ParamUniform : public ParamValues {
public:
	ParamUniform(double min = 0, double max = 1, int count = 5);

	virtual ~ParamUniform() = default;

	virtual ParamValues *Copy() const override;
	virtual wxString GetText() const override;

	double min() const {return min_;}
	double max() const {return max_;}
	int count() const {return count_;}

private:
	double min_;
	double max_;
	int count_;
};

ParamUniform::ParamUniform(double min, double max, int count)
	: ParamValues(kPageUniform)
	, min_(min)
	, max_(max)
	, count_(count)
{}

ParamValues *ParamUniform::Copy() const
{
	return new ParamUniform(*this);
}

wxString ParamUniform::GetText() const
{
	return wxString::Format("[uniform] min: %g, max: %g, count: %d",
							min_, max_, count_);
}

class ParamTreeParameter : public ParamTreeNode {
public:
	ParamTreeParameter();

	virtual ParamTreeNode *Copy() const override;

	virtual wxString GetType() const override {return "parameter";}
	virtual wxString GetName() const override {return name_;}
	virtual wxString GetSummary() const override;

	void set_name(const wxString &name) {name_ = name;}

	unsigned int GetPage() const;
	ParamValues *GetValues() const;
	void SetValues(ParamValues *values);

private:
	wxString name_;
	std::unique_ptr<ParamValues> values_;
};

ParamTreeParameter::ParamTreeParameter()
	: values_(new ParamInterval)
{}

ParamTreeNode *ParamTreeParameter::Copy() const
{
	std::unique_ptr<ParamTreeParameter> copied(new ParamTreeParameter);
	copied->set_name(GetName());
	copied->SetValues(values_->Copy());
	return copied.release();
}

wxString ParamTreeParameter::GetSummary() const
{
	return values_->GetText();
}

unsigned int ParamTreeParameter::GetPage() const
{
	return values_->page();
}

ParamValues *ParamTreeParameter::GetValues() const
{
	return values_.get();
}

void ParamTreeParameter::SetValues(ParamValues *values)
{
	values_.reset(values);
}

ParamTreeParameter *GetParameterFromItem(const wxDataViewItem &item)
{
	auto *node = reinterpret_cast<ParamTreeNode *>(item.GetID());
	if (!node)
		return nullptr;
	return dynamic_cast<ParamTreeParameter *>(node);
}

class ParamTreeProduct : public ParamTreeNode {
public:
	virtual ParamTreeNode *Copy() const override;

	virtual wxString GetType() const override {return "product";}
	virtual wxString GetName() const override {return "";}
	virtual wxString GetSummary() const override {return "";}
};

ParamTreeNode *ParamTreeProduct::Copy() const
{
	return new ParamTreeProduct;
}

class ParamTreeZip : public ParamTreeNode {
public:
	virtual ParamTreeNode *Copy() const override;

	virtual wxString GetType() const override {return "zip";}
	virtual wxString GetName() const override {return "";}
	virtual wxString GetSummary() const override {return "";}
};

ParamTreeNode *ParamTreeZip::Copy() const
{
	return new ParamTreeZip;
}

}

/*
 * This is the data model to be displayed by wxDataViewCtrl.
 */
class ParamTreeViewModel : public wxDataViewModel {
public:
	explicit ParamTreeViewModel(const ParamTree &param_tree);

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

	const std::unordered_set<std::unique_ptr<ParamTreeNode> > &nodes() const {return nodes_;}
	const std::unordered_map<std::uintptr_t, std::uintptr_t> &pm() const {return pm_;}

private:
	std::unordered_set<std::unique_ptr<ParamTreeNode> > nodes_;
	std::unordered_map<std::uintptr_t, std::uintptr_t> pm_; // parent map: id to parent id
};

ParamTreeViewModel::ParamTreeViewModel(const ParamTree &param_tree)
{
	std::unordered_map<std::uintptr_t, std::uintptr_t> m; // old to new
	for (auto nit=param_tree.nodes.begin();nit!=param_tree.nodes.end();++nit) {
		auto p = nodes_.emplace((*nit)->Copy());
		assert(p.second);
		m.emplace(reinterpret_cast<std::uintptr_t>(nit->get()),
				  reinterpret_cast<std::uintptr_t>(p.first->get()));
	}
	for (auto p : param_tree.pm) {
		auto it1 = m.find(p.first);
		assert(it1 != m.end());
		if (p.second) {
			auto it2 = m.find(p.second);
			assert(it2 != m.end());
			pm_.emplace(it1->second, it2->second);
		} else {
			pm_.emplace(it1->second, 0);
		}
	}
}

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
		variant = node->GetSummary();
		break;
	}
}

bool ParamTreeViewModel::IsContainer(const wxDataViewItem &item) const
{
	return !GetParameterFromItem(item);
}

bool ParamTreeViewModel::SetValue(const wxVariant &variant, const wxDataViewItem &item, unsigned int col)
{
	if (col != 1)
		return false;
	auto *parameter = GetParameterFromItem(item);
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

	// remove the subtree having the deleted one as its root
	std::unordered_set<std::uintptr_t> descendants;
	descendants.insert(reinterpret_cast<std::uintptr_t>(node));
	do {
		auto dit = descendants.begin();
		std::uintptr_t d = *dit;
		descendants.erase(dit);
		for (auto nit=nodes_.begin();nit!=nodes_.end();++nit) {
			if (d == reinterpret_cast<std::uintptr_t>(nit->get())) {
				nodes_.erase(nit);
				break;
			}
		}
		auto pit = pm_.begin();
		while (pit != pm_.end()) {
			if (d == pit->second) {
				descendants.insert(pit->first);
				pit = pm_.erase(pit);
			} else {
				++pit;
			}
		}
	} while (!descendants.empty());
}

class ParamIntervalCtrl : public wxPropertyGrid {
public:
	explicit ParamIntervalCtrl(wxWindow *parent);

	void ImportValues(const ParamTreeParameter *parameter);
	void ExportValues(ParamTreeParameter *parameter);

private:
	wxFloatProperty *lower_prop_;
	wxFloatProperty *upper_prop_;
	wxFloatProperty *step_prop_;
};

ParamIntervalCtrl::ParamIntervalCtrl(wxWindow *parent)
	: wxPropertyGrid(parent)
	, lower_prop_(new wxFloatProperty("lower", wxPG_LABEL, 0))
	, upper_prop_(new wxFloatProperty("upper", wxPG_LABEL, 4))
	, step_prop_(new wxFloatProperty("step", wxPG_LABEL, 1))
{
	Append(lower_prop_);
	Append(upper_prop_);
	Append(step_prop_);
	SetMinSize(wxSize(300, 300));
	SetSplitterLeft();
}

void ParamIntervalCtrl::ImportValues(const ParamTreeParameter *parameter)
{
	auto *pi = dynamic_cast<const ParamInterval *>(parameter->GetValues());
	assert(pi);
	lower_prop_->SetValue(pi->lower());
	upper_prop_->SetValue(pi->upper());
	step_prop_->SetValue(pi->step());
}

void ParamIntervalCtrl::ExportValues(ParamTreeParameter *parameter)
{
	parameter->SetValues(new ParamInterval(lower_prop_->GetValue().GetDouble(),
										   upper_prop_->GetValue().GetDouble(),
										   step_prop_->GetValue().GetDouble()));
}

class ParamEnumCtrl : public wxTextCtrl {
public:
	explicit ParamEnumCtrl(wxWindow *parent);

	void ImportValues(const ParamTreeParameter *parameter);
	void ExportValues(ParamTreeParameter *parameter);
};

ParamEnumCtrl::ParamEnumCtrl(wxWindow *parent)
	: wxTextCtrl(parent, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE)
{
	SetMinSize(wxSize(300, 300));
}

void ParamEnumCtrl::ImportValues(const ParamTreeParameter *parameter)
{
	auto *pe = dynamic_cast<const ParamEnum *>(parameter->GetValues());
	assert(pe);
	Clear();
	*this << pe->body();
}

void ParamEnumCtrl::ExportValues(ParamTreeParameter *parameter)
{
	parameter->SetValues(new ParamEnum(GetValue()));
}

class ParamGaussianCtrl : public wxPropertyGrid {
public:
	explicit ParamGaussianCtrl(wxWindow *parent);

	void ImportValues(const ParamTreeParameter *parameter);
	void ExportValues(ParamTreeParameter *parameter);

private:
	wxFloatProperty *mean_prop_;
	wxFloatProperty *stddev_prop_;
	wxIntProperty *count_prop_;
};

ParamGaussianCtrl::ParamGaussianCtrl(wxWindow *parent)
	: wxPropertyGrid(parent)
	, mean_prop_(new wxFloatProperty("mean", wxPG_LABEL, 0))
	, stddev_prop_(new wxFloatProperty("stddev", wxPG_LABEL, 1))
	, count_prop_(new wxIntProperty("count", wxPG_LABEL, 10))
{
	Append(mean_prop_);
	Append(stddev_prop_);
	Append(count_prop_);
	SetMinSize(wxSize(300, 300));
	SetSplitterLeft();
}

void ParamGaussianCtrl::ImportValues(const ParamTreeParameter *parameter)
{
	auto *pg = dynamic_cast<const ParamGaussian *>(parameter->GetValues());
	assert(pg);
	mean_prop_->SetValue(pg->mean());
	stddev_prop_->SetValue(pg->stddev());
	count_prop_->SetValue(pg->count());
}

void ParamGaussianCtrl::ExportValues(ParamTreeParameter *parameter)
{
	parameter->SetValues(new ParamGaussian(mean_prop_->GetValue().GetDouble(),
										   stddev_prop_->GetValue().GetDouble(),
										   static_cast<int>(count_prop_->GetValue().GetLong())));
}

class ParamUniformCtrl : public wxPropertyGrid {
public:
	explicit ParamUniformCtrl(wxWindow *parent);

	void ImportValues(const ParamTreeParameter *parameter);
	void ExportValues(ParamTreeParameter *parameter);

private:
	wxFloatProperty *min_prop_;
	wxFloatProperty *max_prop_;
	wxIntProperty *count_prop_;
};

ParamUniformCtrl::ParamUniformCtrl(wxWindow *parent)
	: wxPropertyGrid(parent)
	, min_prop_(new wxFloatProperty("minimum", wxPG_LABEL, 0))
	, max_prop_(new wxFloatProperty("maximum", wxPG_LABEL, 1))
	, count_prop_(new wxIntProperty("count", wxPG_LABEL, 10))
{
	Append(min_prop_);
	Append(max_prop_);
	Append(count_prop_);
	SetMinSize(wxSize(300, 300));
	SetSplitterLeft();
}

void ParamUniformCtrl::ImportValues(const ParamTreeParameter *parameter)
{
	auto *pu = dynamic_cast<const ParamUniform *>(parameter->GetValues());
	assert(pu);
	min_prop_->SetValue(pu->min());
	max_prop_->SetValue(pu->max());
	count_prop_->SetValue(pu->count());
}

void ParamUniformCtrl::ExportValues(ParamTreeParameter *parameter)
{
	parameter->SetValues(new ParamUniform(min_prop_->GetValue().GetDouble(),
										  max_prop_->GetValue().GetDouble(),
										  static_cast<int>(count_prop_->GetValue().GetLong())));
}

PhspEditorDialog::PhspEditorDialog(wxWindow *parent, ParamTree &param_tree)
	: wxDialog(parent, wxID_ANY, "Edit parameter set")
	, param_tree_(param_tree)
	, model_(new ParamTreeViewModel(param_tree))
	, tree_view_(new wxDataViewCtrl(this, wxID_ANY))
	, book_(new wxChoicebook(this, wxID_ANY))
	, interval_ctrl_(new ParamIntervalCtrl(book_))
	, enum_ctrl_(new ParamEnumCtrl(book_))
	, gaussian_ctrl_(new ParamGaussianCtrl(book_))
	, uniform_ctrl_(new ParamUniformCtrl(book_))
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
	book_->AddPage(interval_ctrl_, "interval");
	book_->AddPage(enum_ctrl_, "enum");
	book_->AddPage(gaussian_ctrl_, "random - Gaussian");
	book_->AddPage(uniform_ctrl_, "random - uniform");
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

	// events
	tree_view_->Bind(wxEVT_DATAVIEW_SELECTION_CHANGED, &PhspEditorDialog::OnSelectionChanged, this);
	book_->Bind(wxEVT_CHOICEBOOK_PAGE_CHANGED, &PhspEditorDialog::OnChoicebookPageChanged, this);
	interval_ctrl_->Bind(wxEVT_PG_CHANGED, &PhspEditorDialog::OnIntervalPgChanged, this);
	enum_ctrl_->Bind(wxEVT_TEXT, &PhspEditorDialog::OnEnumText, this);
	gaussian_ctrl_->Bind(wxEVT_PG_CHANGED, &PhspEditorDialog::OnGaussianPgChanged, this);
	uniform_ctrl_->Bind(wxEVT_PG_CHANGED, &PhspEditorDialog::OnUniformPgChanged, this);
}

void PhspEditorDialog::Save()
{
	param_tree_.Import(model_->nodes(), model_->pm());
}

void PhspEditorDialog::OnApply(wxCommandEvent &)
{
	Save();
}

void PhspEditorDialog::OnPlus(wxCommandEvent &)
{
	auto item = tree_view_->GetCurrentItem();
	// If a parameter is selected, take its parent
	if (item.IsOk() && !model_->IsContainer(item))
		item = model_->GetParent(item);

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

	book_->Enable();
	int selected = book_->GetSelection();
	if (selected == kPageNA)
		book_->ChangeSelection(kPageInterval);
	SpecifyValues(book_->GetSelection());
}

void PhspEditorDialog::OnMinus(wxCommandEvent &)
{
	auto item = tree_view_->GetCurrentItem();
	if (item.IsOk()) {
		auto name = model_->GetItemText(item);
		auto message = wxString::Format("Are you sure to delete %s?", name.empty() ? "it" : name);
		wxMessageDialog dialog(this, message, "Delete it?", wxYES_NO|wxNO_DEFAULT);
		if (dialog.ShowModal() == wxID_YES)
			model_->DeleteItem(item);
	}
}

void PhspEditorDialog::OnSelectionChanged(wxDataViewEvent &event)
{
	auto item = event.GetItem();
	auto *parameter = GetParameterFromItem(item);
	if (parameter) {
		auto page = parameter->GetPage();
		switch (page) {
		case kPageInterval:
			interval_ctrl_->ImportValues(parameter);
			break;
		case kPageEnum:
			enum_ctrl_->ImportValues(parameter);
			break;
		case kPageGaussian:
			gaussian_ctrl_->ImportValues(parameter);
			break;
		case kPageUniform:
			uniform_ctrl_->ImportValues(parameter);
			break;
		default:
			assert(false);
			break;
		}
		book_->ChangeSelection(page);
		book_->Enable();
	} else {
		book_->ChangeSelection(kPageNA);
		book_->Disable();
	}
}

void PhspEditorDialog::OnChoicebookPageChanged(wxBookCtrlEvent &event)
{
	SpecifyValues(event.GetSelection());
}

void PhspEditorDialog::OnIntervalPgChanged(wxPropertyGridEvent &)
{
	auto item = tree_view_->GetSelection();
	if (!item.IsOk())
		return;
	auto *parameter = GetParameterFromItem(item);
	if (!parameter)
		return;
	interval_ctrl_->ExportValues(parameter);
	model_->ItemChanged(item);
}

void PhspEditorDialog::OnEnumText(wxCommandEvent &)
{
	auto item = tree_view_->GetSelection();
	if (!item.IsOk())
		return;
	auto *parameter = GetParameterFromItem(item);
	if (!parameter)
		return;
	enum_ctrl_->ExportValues(parameter);
	model_->ItemChanged(item);
}

void PhspEditorDialog::OnGaussianPgChanged(wxPropertyGridEvent &)
{
	auto item = tree_view_->GetSelection();
	if (!item.IsOk())
		return;
	auto *parameter = GetParameterFromItem(item);
	if (!parameter)
		return;
	gaussian_ctrl_->ExportValues(parameter);
	model_->ItemChanged(item);
}

void PhspEditorDialog::OnUniformPgChanged(wxPropertyGridEvent &)
{
	auto item = tree_view_->GetSelection();
	if (!item.IsOk())
		return;
	auto *parameter = GetParameterFromItem(item);
	if (!parameter)
		return;
	uniform_ctrl_->ExportValues(parameter);
	model_->ItemChanged(item);
}

void PhspEditorDialog::SpecifyValues(int selected)
{
	if (selected == kPageNA)
		return;
	auto item = tree_view_->GetSelection();
	if (!item.IsOk())
		return;
	auto *parameter = GetParameterFromItem(item);
	if (!parameter)
		return;
	switch (selected) {
	case kPageInterval:
		interval_ctrl_->ExportValues(parameter);
		break;
	case kPageEnum:
		enum_ctrl_->ExportValues(parameter);
		break;
	case kPageGaussian:
		gaussian_ctrl_->ExportValues(parameter);
		break;
	case kPageUniform:
		uniform_ctrl_->ExportValues(parameter);
		break;
	default:
		assert(false);
		break;
	}
	model_->ItemChanged(item);
}

}
}
