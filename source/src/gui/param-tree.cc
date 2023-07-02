/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "gui/param-tree.h"

namespace flint {
namespace gui {

ParamValues::ParamValues(size_t page)
	: page_(page)
{}

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

wxString ParamInterval::GetXml(const wxString &name, int indent) const
{
	wxString s;
	for (int i=0;i<indent;i++)
		s << ' ';
	s << "<parameter type='sequence' name='" << name << "'>\n";
	for (int i=0;i<indent+2;i++)
		s << ' ';
	s << "<range type='interval' lower='" << lower_ << "' upper='" << upper_ << "' step='" << step_ << "' />\n";
	for (int i=0;i<indent;i++)
		s << ' ';
	s << "</parameter>\n";
	return s;
}

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

wxString ParamEnum::GetXml(const wxString &name, int indent) const
{
	wxString s;
	for (int i=0;i<indent;i++)
		s << ' ';
	s << "<parameter type='sequence' name='" << name << "'>\n";
	for (int i=0;i<indent+2;i++)
		s << ' ';
	s << "<range type='enum'>" << body_ << "</range>\n";
	for (int i=0;i<indent;i++)
		s << ' ';
	s << "</parameter>\n";
	return s;
}

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

wxString ParamGaussian::GetXml(const wxString &name, int indent) const
{
	wxString s;
	for (int i=0;i<indent;i++)
		s << ' ';
	s << "<parameter type='random' name='" << name << "'>\n";
	for (int i=0;i<indent+2;i++)
		s << ' ';
	s << "<gaussian mean='" << mean_ << "' stddev='" << stddev_ << "' count='" << count_ << "' />\n";
	for (int i=0;i<indent;i++)
		s << ' ';
	s << "</parameter>\n";
	return s;
}

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

wxString ParamUniform::GetXml(const wxString &name, int indent) const
{
	wxString s;
	for (int i=0;i<indent;i++)
		s << ' ';
	s << "<parameter type='random' name='" << name << "'>\n";
	for (int i=0;i<indent+2;i++)
		s << ' ';
	s << "<uniform min='" << min_ << "' max='" << max_ << "' count='" << count_ << "' />\n";
	for (int i=0;i<indent;i++)
		s << ' ';
	s << "</parameter>\n";
	return s;
}

ParamTreeNode::~ParamTreeNode() = default;

ParamTree::~ParamTree() = default;

void ParamTree::Import(const std::unordered_set<std::unique_ptr<ParamTreeNode> > &given_nodes,
					   const std::unordered_map<std::uintptr_t, std::uintptr_t> &given_pm)
{
	pm.clear();
	nodes.clear();

	std::unordered_map<std::uintptr_t, std::uintptr_t> m; // old to new
	for (auto nit=given_nodes.begin();nit!=given_nodes.end();++nit) {
		auto p = nodes.emplace((*nit)->Copy());
		assert(p.second);
		m.emplace(reinterpret_cast<std::uintptr_t>(nit->get()),
				  reinterpret_cast<std::uintptr_t>(p.first->get()));
	}
	for (auto p : given_pm) {
		auto it1 = m.find(p.first);
		assert(it1 != m.end());
		if (p.second) {
			auto it2 = m.find(p.second);
			assert(it2 != m.end());
			pm.emplace(it1->second, it2->second);
		} else {
			pm.emplace(it1->second, 0);
		}
	}
}

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

wxString ParamTreeParameter::GetXml(int indent) const
{
	return values_->GetXml(name_, indent);
}

ParamTreeNode *ParamTreeProduct::Copy() const
{
	return new ParamTreeProduct;
}

ParamTreeNode *ParamTreeZip::Copy() const
{
	return new ParamTreeZip;
}

}
}
