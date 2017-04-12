/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_PARAM_TREE_H_
#define FLINT_GUI_PARAM_TREE_H_

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <unordered_set>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

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
	virtual wxString GetXml(const wxString &name, int indent) const = 0;

protected:
	explicit ParamValues(size_t page);

private:
	unsigned int page_;
};

class ParamInterval : public ParamValues {
public:
	ParamInterval(double lower = 0,
				  double upper = 4,
				  double step = 1);

	virtual ~ParamInterval() = default;

	virtual ParamValues *Copy() const override;
	virtual wxString GetText() const override;
	virtual wxString GetXml(const wxString &name, int indent) const;

	double lower() const {return lower_;}
	double upper() const {return upper_;}
	double step() const {return step_;}

private:
	double lower_;
	double upper_;
	double step_;
};

class ParamEnum : public ParamValues {
public:
	explicit ParamEnum(const wxString &body);

	virtual ~ParamEnum() = default;

	virtual ParamValues *Copy() const override;
	virtual wxString GetText() const override;
	virtual wxString GetXml(const wxString &name, int indent) const;

	const wxString &body() const {return body_;}

private:
	wxString body_;
};

class ParamGaussian : public ParamValues {
public:
	ParamGaussian(double mean = 0, double stddev = 1, int count = 5);

	virtual ~ParamGaussian() = default;

	virtual ParamValues *Copy() const override;
	virtual wxString GetText() const override;
	virtual wxString GetXml(const wxString &name, int indent) const;

	double mean() const {return mean_;}
	double stddev() const {return stddev_;}
	int count() const {return count_;}

private:
	double mean_;
	double stddev_;
	int count_;
};

class ParamUniform : public ParamValues {
public:
	ParamUniform(double min = 0, double max = 1, int count = 5);

	virtual ~ParamUniform() = default;

	virtual ParamValues *Copy() const override;
	virtual wxString GetText() const override;
	virtual wxString GetXml(const wxString &name, int indent) const;

	double min() const {return min_;}
	double max() const {return max_;}
	int count() const {return count_;}

private:
	double min_;
	double max_;
	int count_;
};

class ParamTreeNode {
public:
	virtual ~ParamTreeNode();

	virtual ParamTreeNode *Copy() const = 0;

	virtual wxString GetType() const = 0;
	virtual wxString GetName() const = 0;
	virtual wxString GetSummary() const = 0;
};

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

	wxString GetXml(int indent) const;

private:
	wxString name_;
	std::unique_ptr<ParamValues> values_;
};

class ParamTreeProduct : public ParamTreeNode {
public:
	virtual ParamTreeNode *Copy() const override;

	virtual wxString GetType() const override {return "product";}
	virtual wxString GetName() const override {return "";}
	virtual wxString GetSummary() const override {return "";}
};

class ParamTreeZip : public ParamTreeNode {
public:
	virtual ParamTreeNode *Copy() const override;

	virtual wxString GetType() const override {return "zip";}
	virtual wxString GetName() const override {return "";}
	virtual wxString GetSummary() const override {return "";}
};

struct ParamTree {
	~ParamTree();

	void Import(const std::unordered_set<std::unique_ptr<ParamTreeNode> > &given_nodes,
				const std::unordered_map<std::uintptr_t, std::uintptr_t> &given_pm);

	std::unordered_set<std::unique_ptr<ParamTreeNode> > nodes;
	std::unordered_map<std::uintptr_t, std::uintptr_t> pm; // parent map: id to parent id
};

// parent id to child id
using ParamMap = std::unordered_multimap<std::uintptr_t, std::uintptr_t>;

}
}

#endif
