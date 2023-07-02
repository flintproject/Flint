/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

#include <libxml/xmlreader.h>

#include "branch.h"
#include "db.h"
#include "db/helper.h"
#include "db/query.h"
#include "db/utility.h"
#include "flint/utf8string.h"
#include "modelpath.h"
#include "phml/bridge.h"
#include "phml/database-driver.h"
#include "phml/definition.h"
#include "phml/definition_dumper.h"
#include "phml/delay-arg-validator.h"
#include "phml/edge.h"
#include "phml/element.h"
#include "phml/extra-implementation.h"
#include "phml/graph-iv-rewriter.h"
#include "phml/graph-math-rewriter.h"
#include "phml/graph-reader.h"
#include "phml/implementation.h"
#include "phml/import.h"
#include "phml/initial-value.h"
#include "phml/instance.h"
#include "phml/module.h"
#include "phml/numerical-configuration.h"
#include "phml/port.h"
#include "phml/pq.h"
#include "phml/reference.h"
#include "phml/target-module.h"
#include "phml/target-pq.h"
#include "phml/template.h"
#include "phml/time-discretization.h"
#include "phml/timeseries.h"
#include "phml/transition-form.h"
#include "phml/tsipc.h"
#include "phml/unit.h"
#include "phml/validator.h"
#include "reach.h"
#include "span.h"
#include "sprinkle.h"
#include "sqlite3.h"
#include "ts.h"

namespace flint {
namespace phml {

namespace {

class CapsulatedByValidator {
public:
	CapsulatedByValidator()
		: query_stmt_(nullptr)
	{
	}

	~CapsulatedByValidator() {
		sqlite3_finalize(query_stmt_);
	}

	bool Validate(sqlite3 *db) {
		static const char kQuery[] =
			"SELECT m0.module_id, m0.capsulated_by FROM modules AS m0 WHERE"
			" m0.capsulated_by IS NOT NULL AND"
			" NOT EXISTS (SELECT * FROM modules AS m1 WHERE m1.module_id = m0.capsulated_by)";

		bool r = true;
		int e = db::PrepareStatement(db, kQuery, &query_stmt_);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << kQuery << ": " << e << std::endl;
			return false;
		}
		for (e = sqlite3_step(query_stmt_); e == SQLITE_ROW; e = sqlite3_step(query_stmt_)) {
			r = false;
			const void *module_id = sqlite3_column_blob(query_stmt_, 0);
			const void *capsulated_by = sqlite3_column_blob(query_stmt_, 1);
			assert(module_id);
			assert(capsulated_by);
			boost::uuids::uuid mu, cu;
			std::memcpy(&mu, module_id, mu.size());
			std::memcpy(&cu, capsulated_by, cu.size());
			std::cerr << "module of module-id " << mu
				 << " is capsulated by unknown capsule module: " << cu
				 << std::endl;
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << kQuery << ": " << e << std::endl;
			return false;
		}
		sqlite3_reset(query_stmt_);
		return r;
	}

private:
	sqlite3_stmt *query_stmt_;
};

class TreeWriter {
public:
	TreeWriter(const TreeWriter &) = delete;
	TreeWriter &operator=(const TreeWriter &) = delete;

	TreeWriter()
		: query_stmt_(nullptr)
		, tree_stmt_(nullptr)
		, roots_()
		, children_()
	{
	}

	~TreeWriter() {
		sqlite3_finalize(query_stmt_);
		sqlite3_finalize(tree_stmt_);
	}

	bool Write(sqlite3 *db) {
		int e;
		e = db::PrepareStatement(db, "SELECT module_id, capsulated_by FROM modules WHERE type = 'capsule' OR type = 'functional-unit'",
								 &query_stmt_);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			return false;
		}
		e = db::PrepareStatement(db, "INSERT INTO trees VALUES (?, ?)",
								 &tree_stmt_);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			return false;
		}
		for (e = sqlite3_step(query_stmt_); e == SQLITE_ROW; e = sqlite3_step(query_stmt_)) {
			const void *module_id = sqlite3_column_blob(query_stmt_, 0);
			assert(module_id);
			assert(sqlite3_column_bytes(query_stmt_, 0) == boost::uuids::uuid::static_size());
			const void *capsulated_by = sqlite3_column_blob(query_stmt_, 1);
			boost::uuids::uuid child;
			std::memcpy(&child, module_id, child.size());
			if (capsulated_by) {
				assert(sqlite3_column_bytes(query_stmt_, 1) == boost::uuids::uuid::static_size());
				boost::uuids::uuid parent;
				std::memcpy(&parent, capsulated_by, parent.size());
				children_.emplace(parent, child);
			} else {
				roots_.push_back(child);
			}
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(query_stmt_);

		for (const auto &v : roots_) {
			if (!SaveDescendants(v, 0))
				return false;
		}
		return true;
	}

private:
	typedef std::multimap<boost::uuids::uuid, boost::uuids::uuid> Multimap;
	typedef std::vector<boost::uuids::uuid> Vector;

	bool SaveDescendants(const boost::uuids::uuid &parent, int level) {
		std::pair<Multimap::const_iterator, Multimap::const_iterator> p;
		p = children_.equal_range(parent);
		for (Multimap::const_iterator it=p.first;it!=p.second;++it) {
			if (!SaveDescendants(it->second, level + 1)) return false;
		}
		return Save(parent, level);
	}

	bool Save(const boost::uuids::uuid &module_id, int level) {
		int e = sqlite3_bind_blob(tree_stmt_, 1, &module_id, module_id.size(), SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind module_id: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_int64(tree_stmt_, 2, level);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind level: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(tree_stmt_);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(tree_stmt_);
		return true;
	}

	sqlite3_stmt *query_stmt_;
	sqlite3_stmt *tree_stmt_;
	Vector roots_;
	Multimap children_;
};

class Reader {
public:
	Reader(const Reader &) = delete;
	Reader &operator=(const Reader &) = delete;

	Reader(const boost::filesystem::path &given_path,
		   const boost::filesystem::path &model_path,
		   xmlTextReaderPtr &text_reader,
		   sqlite3 *db)
		: given_path_(given_path),
		  model_path_(model_path),
		  text_reader_(text_reader),
		  dd_(new DatabaseDriver(db)),
		  module_(),
		  pq_(),
		  iv_(),
		  impl_(),
		  ref_(),
		  extra_(),
		  edge_(),
		  bridge_(),
		  instance_(),
		  iv_dumper_(),
		  impl_dumper_(),
		  extra_dumper_()
		, is_Wiener_process_(false)
	{
	}

	~Reader() {
		xmlFreeTextReader(text_reader_);
		xmlCleanupParser();
	}

	int Read() {
		if (!dd_->Initialize())
			return -2;

		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("header"))) {
					i = ReadHeader();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("unit-set"))) {
					i = ReadUnitSet();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("module-set"))) {
					i = ReadModuleSet();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("template-set"))) {
					i = ReadTemplateSet();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("instance-set"))) {
					i = ReadInstanceSet();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("edge-set"))) {
					i = ReadEdgeSet();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("edge-set"))) {
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

private:
	void ReportHint() {
		if (module_ && module_->module_id() && module_->name()) {
			std::cerr << " at";
			if (pq_ && pq_->pq_id() > 0 && pq_->name()) {
				std::cerr << " physical-quantity \"" << pq_->name()
					 << "\" (" << pq_->pq_id() << ") of";
			}
			std::cerr << " module \"" << module_->name()
				 << "\" (" << module_->module_id()
				 << ')' << std::endl;
		}
	}

	int ReadHeader() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) // ignore empty element
			return xmlTextReaderRead(text_reader_);
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("numerical-configuration"))) {
					i = ReadNumericalConfiguration();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("header"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadNumericalConfiguration() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) // ignore empty element
			return xmlTextReaderRead(text_reader_);
		std::unique_ptr<NumericalConfiguration> nc(new NumericalConfiguration);
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("time-discretization"))) {
					i = ReadTimeDiscretization();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("algorithm"))) {
					i = ReadAlgorithm(nc.get());
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("simulation-time-span"))) {
					i = ReadSimulationTimeSpan(nc.get());
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <numerical-configuration>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("numerical-configuration"))) {
					if (!dd_->SaveNumericalConfiguration(nc.get())) return -2;
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadTimeDiscretization() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) {
			// no children, so we can ignore this <time-discretization>
			return xmlTextReaderRead(text_reader_);
		}
		std::unique_ptr<TimeDiscretization> td(new TimeDiscretization);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("unit-id"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				int unit_id = std::atoi(reinterpret_cast<const char *>(value));
				if (unit_id > 0) {
					td->set_unit_id(unit_id);
				} else {
					// generously ignore invalid unit-id
					std::cerr << "warning: invalid unit-id of <time-discretization>: "
						 << unit_id << std::endl;
				}
			}
		}
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("step"))) {
					i = ReadStep(td.get());
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("time-discretization"))) {
					if (td->unit_id() && td->step()) {
						if (!dd_->SaveTimeDiscretization(td.get(), module_.get())) return -2;
					}
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadStep(TimeDiscretization *td) {
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		if (!s) {
			// generously ignore empty step
			std::cerr << "warning: missing body of <step>" << std::endl;
			return xmlTextReaderNext(text_reader_);
		}
		xmlChar *step;
		if (!Trim(s, &step)) {
			xmlFree(s);
			return -2;
		}
		int len = xmlStrlen(step);
		if (len == 0) {
			// generously ignore empty step
			std::cerr << "warning: empty body of <step>" << std::endl;
			xmlFree(s);
			return xmlTextReaderNext(text_reader_);
		}

		// validate step
		if (ContainNonGraphic(step)) {
			std::cerr << "<step> contains invalid character: \"" << step << "\"" << std::endl;
			xmlFree(s);
			return -2;
		}

		td->set_step(xmlStrdup(step));
		xmlFree(s);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadAlgorithm(NumericalConfiguration *nc) {
		if (xmlTextReaderIsEmptyElement(text_reader_)) // ignore empty element
			return xmlTextReaderRead(text_reader_);
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("random-generator"))) {
					i = ReadRandomGenerator(nc);
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("integration"))) {
					i = ReadIntegration(nc);
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <algorithm>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("algorithm"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadRandomGenerator(NumericalConfiguration *nc) {
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				// ignored
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("name"))) {
				nc->set_rg_name(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("seed"))) {
				nc->set_rg_seed(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <random-generator>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!nc->rg_name()) {
			std::cerr << "missing name of <random-generator>" << std::endl;
			return -2;
		}
		return xmlTextReaderNext(text_reader_);
	}

	int ReadIntegration(NumericalConfiguration *nc) {
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("name"))) {
				nc->set_integration(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <integration>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!nc->integration()) {
			std::cerr << "missing name of <integration>" << std::endl;
			return -2;
		}
		return xmlTextReaderNext(text_reader_);
	}

	int ReadSimulationTimeSpan(NumericalConfiguration *nc) {
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("unit-id"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				int unit_id = std::atoi(reinterpret_cast<const char *>(value));
				if (unit_id <= 0) {
					std::cerr << "invalid unit-id of <simulation-time-span>: " << value << std::endl;
					return -2;
				}
				nc->set_sts_unit_id(unit_id);
			}
		}
		if (!nc->sts_unit_id()) {
			std::cerr << "missing unit-id of <simulation-time-span>" << std::endl;
			return -2;
		}

		i = xmlTextReaderRead(text_reader_);
		if (i <= 0) return i;
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		assert(s);

		xmlChar *sts_value;
		if (!Trim(s, &sts_value)) {
			xmlFree(s);
			return -2;
		}
		int len = xmlStrlen(sts_value);
		if (len == 0) {
			// generously ignore empty simulation-time-span
			std::cerr << "warning: empty body of <simulation-time-span>" << std::endl;
			xmlFree(s);
			return xmlTextReaderNext(text_reader_);
		}
		if (ContainNonGraphic(sts_value)) {
			std::cerr << "<simulation-time-span> contains invalid character: \"" << sts_value << "\"" << std::endl;
			xmlFree(s);
			return -2;
		}
		nc->set_sts_value(xmlStrdup(sts_value));
		xmlFree(s);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadUnitSet() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) // ignore empty element
			return xmlTextReaderRead(text_reader_);
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("unit"))) {
					i = ReadUnit();
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <unit-set>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("unit-set"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadUnit() {
		xmlChar *value;
		int i = ReadAttributeValue(reinterpret_cast<const xmlChar *>("unit-id"), &value);
		if (i <= 0) {
			std::cerr << "missing unit-id of <unit>" << std::endl;
			return -2;
		}
		int unit_id = std::atoi(reinterpret_cast<const char *>(value));
		xmlFree(value);
		if (unit_id < 0) {
			std::cerr << "invalid unit-id " << unit_id << std::endl;
			return -2;
		}
		std::unique_ptr<Unit> unit(new Unit(unit_id));
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("name"))) {
					i = ReadUnitName(unit.get());
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("element"))) {
					i = ReadElement(unit.get());
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <unit>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("unit"))) {
					if (!unit->name()) {
						std::cerr << "missing <name> of <unit>" << std::endl;
						return -2;
					}
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadUnitName(Unit *unit) {
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		assert(s);

		// validate name
		int len = xmlStrlen(s);
		for (int i=0;i<len;i++) {
			if (!isprint(s[i])) {
				std::cerr << "unit name contains invalid character: \"" << s << "\"" << std::endl;
				xmlFree(s);
				return -2;
			}
		}

		unit->set_name(s);
		if (!dd_->SaveUnit(unit)) return -2;
		return xmlTextReaderNext(text_reader_);
	}

	int ReadElement(const Unit *unit) {
		if (!unit->rowid()) {
			std::cerr << "<element> comes before <name> of <unit>" << std::endl;
			return -2;
		}

		std::unique_ptr<Element> element(new Element);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			if (!value) continue;

			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("unit-id"))) {
				int unit_id = std::atoi(reinterpret_cast<const char *>(value));
				if (unit_id < 0) {
					std::cerr << "invalid unit-id of <element>" << std::endl;
					return -2;
				}
				if (unit_id == unit->unit_id()) {
					std::cerr << "unit with unit-id " << unit_id << " is ill-defined by <element> with its own unit-id" << std::endl;
					return -2;
				}
				element->set_unit_id(unit_id);
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("exponent"))) {
				element->set_exponent(strtod(reinterpret_cast<const char *>(value), nullptr));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("prefix"))) {
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("yotta"))) {
					element->set_factor(24);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("zetta"))) {
					element->set_factor(21);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("exa"))) {
					element->set_factor(18);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("peta"))) {
					element->set_factor(15);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("tera"))) {
					element->set_factor(12);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("giga"))) {
					element->set_factor(9);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("mega"))) {
					element->set_factor(6);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("kilo"))) {
					element->set_factor(3);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("hecto"))) {
					element->set_factor(2);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("deca"))) {
					element->set_factor(1);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("deci"))) {
					element->set_factor(-1);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("centi"))) {
					element->set_factor(-2);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("milli"))) {
					element->set_factor(-3);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("micro"))) {
					element->set_factor(-6);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("nano"))) {
					element->set_factor(-9);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("pico"))) {
					element->set_factor(-12);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("femto"))) {
					element->set_factor(-15);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("atto"))) {
					element->set_factor(-18);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("zepto"))) {
					element->set_factor(-21);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("yocto"))) {
					element->set_factor(-24);
				} else {
					std::cerr << "unknown prefix of <element>: " << value << std::endl;
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("multiplier"))) {
				element->set_multiplier(strtod(reinterpret_cast<const char *>(value), nullptr));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("offset"))) {
				element->set_offset(strtod(reinterpret_cast<const char *>(value), nullptr));
			} else {
				std::cerr << "unknown attribute of <element>: " << local_name << std::endl;
				return -2;
			}
		}

		if (element->unit_id() < 0) {
			std::cerr << "missing unit-id of <element> in unit with unit-id "
				 << unit->unit_id() << std::endl;
			return -2;
		}
		if (!dd_->SaveElement(unit, element.get())) return -2;
		return xmlTextReaderNext(text_reader_);
	}

	int ReadModuleSet() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) {
			// no modules, so we have done
			return xmlTextReaderRead(text_reader_);
		}
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("module"))) {
					i = ReadModule();
					if (i <= 0) {
						ReportHint();
						return i;
					}
					continue;
				} else {
					std::cerr << "unknown child of <module-set>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("module-set"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadModule() {
		module_.reset(new Module);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("module-id"))) {
				module_->set_module_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				module_->set_type(xmlTextReaderValue(text_reader_));
			}
		}
		if (!module_->module_id()) {
			std::cerr << "missing module-id of <module>" << std::endl;
			return -2;
		}
		if (!module_->type()) {
			std::cerr << "missing type of <module>: " << module_->module_id() << std::endl;
			return -2;
		}
		bool property_found = false;
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("property"))) {
					property_found = true;
					i = ReadProperty();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("port"))) {
					if (!property_found) {
						ReportNoLeadingProperty();
						return -2;
					}
					i = ReadPort();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("physical-quantity"))) {
					if (!property_found) {
						ReportNoLeadingProperty();
						return -2;
					}
					i = ReadPQ();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("import"))) {
					if (!property_found) {
						ReportNoLeadingProperty();
						return -2;
					}
					i = ReadImport();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("timeseries"))) {
					if (!property_found) {
						ReportNoLeadingProperty();
						return -2;
					}
					i = ReadTimeseries();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("module"))) {
					module_.reset();
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	void ReportNoLeadingProperty() {
		std::cerr << "<property> must come first in <module>: " << module_->module_id() << std::endl;
	}

	int ReadProperty() {
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("name"))) {
					i = ReadModuleName();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("capsulation"))) {
					i = ReadCapsulation();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("template"))) {
					i = ReadModuleTemplate();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("time-discretization"))) {
					i = ReadTimeDiscretization();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("property"))) {
					if (!dd_->SaveModule(module_.get())) return -2;
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadModuleName() {
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		assert(s);

		// validate name
		int len = xmlStrlen(s);
		for (int i=0;i<len;i++) {
			switch (s[i]) {
			case '\n':
			case '\r':
				std::cerr << "name contains invalid character: \"" << s << "\"" << std::endl;
				xmlFree(s);
				return -2;
			default:
				break;
			}
		}

		module_->set_name(s);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadCapsulation() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) {
			// no children, so we have done
			return xmlTextReaderRead(text_reader_);
		}
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("capsulated-by"))) {
					i = ReadCapsulatedBy();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("capsulation"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadCapsulatedBy() {
		xmlChar *module_id;
		int i = ReadAttributeValue(reinterpret_cast<const xmlChar *>("module-id"), &module_id);
		if (i <= 0) {
			std::cerr << "missing module-id of <capsulated-by>" << std::endl;
			return -2;
		}
		module_->set_capsulated_by(module_id);
		return xmlTextReaderRead(text_reader_);
	}

	int ReadModuleTemplate() {
		xmlChar *state;
		int i = ReadAttributeValue(reinterpret_cast<const xmlChar *>("state"), &state);
		if (i <= 0) {
			std::cerr << "missing state of <template>" << std::endl;
			return -2;
		}
		module_->set_template_state(state);
		return xmlTextReaderRead(text_reader_);
	}

	int ReadPort() {
		std::unique_ptr<Port> port(new Port);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("port-id"))) {
				port->set_port_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("direction"))) {
				xmlChar *direction = xmlTextReaderValue(text_reader_);
				if (xmlStrEqual(direction, reinterpret_cast<const xmlChar *>("in"))) {
					port->set_direction(direction);
				} else if (xmlStrEqual(direction, reinterpret_cast<const xmlChar *>("out"))) {
					port->set_direction(direction);
				} else {
					std::cerr << "unknown direction of <port>: " << direction << std::endl;
					xmlFree(direction);
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("ref-physical-quantity-id"))) {
				port->set_ref_pq_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("multiple"))) {
				port->set_multiple(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <port>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!port->port_id()) {
			std::cerr << "missing port-id of <port>" << std::endl;
			return -2;
		}
		if (!dd_->SavePort(module_.get(), port.get())) return -2;
		return xmlTextReaderNext(text_reader_);
	}

	int ReadPQ() {
		pq_.reset(new PQ);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("physical-quantity-id"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				int pq_id = std::atoi(reinterpret_cast<const char *>(value));
				if (pq_id <= 0) {
					std::cerr << "invalid physical-quantity-id of <physical-quantity>: " << value << std::endl;
					return -2;
				}
				pq_->set_pq_id(pq_id);
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("state"))) {
					pq_->set_type(PQ::kState);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("variable-parameter"))) {
					pq_->set_type(PQ::kVariableParameter);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("static-parameter"))) {
					pq_->set_type(PQ::kStaticParameter);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("func-expression"))) {
					// skip this type of PQ
					return i;
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("nominal"))) {
					// skip this type of PQ
					return i;
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("morphology"))) {
					// skip this type of PQ
					return i;
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("timeseries"))) {
					pq_->set_type(PQ::kTimeseries);
				} else {
					std::cerr << "unknown type fo <physical-quantity>: " << value << std::endl;
					return -2;
				}
			}
		}
		if (pq_->pq_id() == 0) {
			std::cerr << "missing physical-quantity-id of <physical-quantity>" << std::endl;
			return -2;
		}
		if (pq_->type() == PQ::kUnknown) {
			std::cerr << "missing type of <physical-quantity>: " << pq_->pq_id() << std::endl;
			return -2;
		}
		if (!dd_->SavePq(module_.get(), pq_.get())) return -2;
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("name"))) {
					i = ReadPqName();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("dimension"))) {
					i = ReadDimension();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("max-delay"))) {
					i = ReadMaxDelay();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("value-type-set"))) {
					i = ReadValueTypeSet();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("initial-value"))) {
					i = ReadInitialValue();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("implementation"))) {
					i = ReadImplementation();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("physical-quantity"))) {
					if (!pq_->name()) {
						std::cerr << "missing <name> of <physical-quantity>: " << pq_->pq_id() << std::endl;
						return -2;
					}
					if (pq_->col() == 0 && pq_->row() == 0) {
						std::cerr << "missing <dimension> of <physical-quantity>" << std::endl;
						return -2;
					}
					if (!dd_->UpdatePq(pq_.get(),
									   is_Wiener_process_ ||
									   (iv_dumper_ && iv_dumper_->TargetIsIndependent()) ||
									   (impl_dumper_ && impl_dumper_->TargetIsIndependent())))
						return -2;
					if (iv_dumper_) {
						if (pq_->type() != PQ::kState) {
							std::cerr << "unexpected <initial-value> for <physical-quantity> of type "
								 << pq_->GetTypeName()
								 << std::endl;
							return -2;
						}
						if (iv_->IsProper() && !dd_->SaveInitialValue(pq_.get(), iv_.get())) return -2;
						iv_dumper_.reset();
					}
					if (impl_dumper_) {
						if (impl_->IsProper() && !dd_->SaveImplementation(pq_.get(), impl_.get())) return -2;
						impl_dumper_.reset();
					}
					if (is_Wiener_process_) {
						if (pq_->type() != PQ::kState) {
							std::cerr << "unexpected Wiener process for <physical-quantity> of type "
								 << pq_->GetTypeName()
								 << std::endl;
							return -2;
						}
						iv_.reset(new InitialValue);
						iv_->Print(" (eq %");
						iv_->Print(pq_->name());
						iv_->Print(" 0)");
						if (!dd_->SaveInitialValue(pq_.get(), iv_.get()))
							return -2;
						impl_.reset(new Implementation);
						impl_->Print(" (Wiener %");
						impl_->Print(pq_->name());
						impl_->Print(")");
						if (!dd_->SaveImplementation(pq_.get(), impl_.get()))
							return -2;
						is_Wiener_process_ = false;
					}
					if (ref_) {
						if (!dd_->SaveReference(pq_.get(), ref_.get(), extra_.get())) return -2;
						ref_.reset();
					}
					if (extra_dumper_) {
						if (extra_->IsProper() && !dd_->SaveExtraImplementation(pq_.get(), extra_.get())) return -2;
						extra_dumper_.reset();
					}
					extra_.reset();
					if (bridge_) {
						if (!dd_->SaveBridge(pq_.get(), bridge_.get())) return -2;
						bridge_.reset();
					}
					pq_.reset();
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadPqName() {
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		assert(s);

		xmlChar *name;
		if (!Trim(s, &name)) {
			xmlFree(s);
			return -2;
		}
		if (xmlStrlen(name) == 0) {
			std::cerr << "empty name of <physical-quantity>" << std::endl;
			xmlFree(s);
			return -2;
		}
		if (ContainNonGraphic(name)) {
			std::cerr << "<physical-quantity>'s name contains invalid character: \""
				 << s
				 << "\""
				 << std::endl;
			xmlFree(s);
			return -2;
		}

		pq_->set_name(xmlStrdup(name));
		xmlFree(s);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadDimension() {
		bool isEmpty = xmlTextReaderIsEmptyElement(text_reader_);
		char dimension_type[7];
		std::memset(dimension_type, 0, 7);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("scalar"))) {
					pq_->set_col(1);
					pq_->set_row(1);
					snprintf(dimension_type, 7, "%s", reinterpret_cast<const char *>(value));
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("vector"))) {
					if (isEmpty) {
						std::cerr << "empty <dimension> of type vector" << std::endl;
						return -2;
					}
					snprintf(dimension_type, 7, "%s", reinterpret_cast<const char *>(value));
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("matrix"))) {
					if (isEmpty) {
						std::cerr << "empty <dimension> of type matrix" << std::endl;
						return -2;
					}
					snprintf(dimension_type, 7, "%s", reinterpret_cast<const char *>(value));
				} else {
					std::cerr << "invalid type of <dimension>: " << value << std::endl;
					return -2;
				}
			}
		}
		if (dimension_type[0] == '\0') {
			std::cerr << "missing type of <dimension>" << std::endl;
			return -2;
		}
		i = xmlTextReaderRead(text_reader_);
		if (isEmpty)
			return i;
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("col"))) {
					i = ReadCol();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("row"))) {
					i = ReadRow();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("dimension"))) {
					if (strcmp(dimension_type, "scalar") == 0) {
						if (pq_->col() * pq_->row() != 1) {
							std::cerr << "<dimension> of type scalar with col*row: "
								 << pq_->col() << '*' << pq_->row()
								 << std::endl;
							return -2;
						}
					} else {
						if (pq_->col() == 0) {
							std::cerr << "missing <col> in <dimension> of type " << dimension_type << std::endl;
							return -2;
						}
						if (pq_->row() == 0) {
							std::cerr << "missing <row> in <dimension> of type " << dimension_type << std::endl;
							return -2;
						}
						if (std::strcmp(dimension_type, "vector") == 0) {
							if (pq_->col() * pq_->row() != std::max(pq_->col(), pq_->row())) {
								std::cerr << "either <col> or <row> of <dimension> of type vector must be 1" << std::endl;
								return -2;
							}
						}
					}
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadCol() {
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		assert(s);

		xmlChar *col;
		if (!Trim(s, &col)) {
			xmlFree(s);
			return -2;
		}
		if (xmlStrlen(col) == 0) {
			std::cerr << "empty <col>" << std::endl;
			xmlFree(s);
			return -2;
		}
		if (xmlStrEqual(col, reinterpret_cast<const xmlChar *>("discretization-dependent"))) {
			std::cerr << "unsupported discretization-dependent <col>" << std::endl;
			xmlFree(s);
			return -2;
		}

		int n = std::atoi(reinterpret_cast<const char *>(col));
		if (n <= 0) {
			std::cerr << "invalid value of <col>: " << col << std::endl;
			xmlFree(s);
			return -2;
		}
		pq_->set_col(n);
		xmlFree(s);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadRow() {
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		assert(s);

		xmlChar *row;
		if (!Trim(s, &row)) {
			xmlFree(s);
			return -2;
		}
		if (xmlStrlen(row) == 0) {
			std::cerr << "empty <row>" << std::endl;
			xmlFree(s);
			return -2;
		}
		if (xmlStrEqual(row, reinterpret_cast<const xmlChar *>("discretization-dependent"))) {
			std::cerr << "unsupported discretization-dependent <row>" << std::endl;
			xmlFree(s);
			return -2;
		}

		int n = std::atoi(reinterpret_cast<const char *>(row));
		if (n <= 0) {
			std::cerr << "invalid value of <row>: " << row << std::endl;
			xmlFree(s);
			return -2;
		}
		pq_->set_row(n);
		xmlFree(s);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadMaxDelay() {
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		assert(s);

		xmlChar *max_delay;
		if (!Trim(s, &max_delay)) {
			xmlFree(s);
			return -2;
		}
		if (xmlStrlen(max_delay) == 0) {
			std::cerr << "warning: empty <max-delay> of <physical-quantity> in module "
				 << module_->module_id()
				 << std::endl;
			xmlFree(s);
			// We do not want to raise the error for old models, so just skip it
			return xmlTextReaderNext(text_reader_);
		}
		if (ContainNonGraphic(max_delay)) {
			std::cerr << "<max-delay> contains invalid character: \""
				 << max_delay
				 << "\""
				 << std::endl;
			xmlFree(s);
			return -2;
		}

		pq_->set_max_delay(xmlStrdup(max_delay));
		xmlFree(s);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadValueTypeSet() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) {
			std::cerr << "missing <value-type>" << std::endl;
			return -2;
		}
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("value-type"))) {
					i = ReadValueType();
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <value-type-set>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("value-type-set"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadValueType() {
		xmlChar *unit_id;
		int i = ReadAttributeValue(reinterpret_cast<const xmlChar *>("unit-id"), &unit_id);
		if (i <= 0) {
			std::cerr << "missing unit-id of <value-type>" << std::endl;
			return -2;
		}
		pq_->set_unit_id(unit_id); // TODO: check invalid values
		return xmlTextReaderRead(text_reader_);
	}

	int ReadInitialValue() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) {
			// no children, we can ignore it safely.
			return xmlTextReaderRead(text_reader_);
		}
		iv_.reset(new InitialValue);
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("definition"))) {
					iv_dumper_.reset(new DefinitionDumper<InitialValue>(text_reader_,
																			  pq_->name(),
																			  iv_.get()));
					i = iv_dumper_->Read(0);
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child element of <initial-value>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("initial-value"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadImplementation() {
		impl_.reset(new Implementation);
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("definition"))) {
					i = ReadDefinitionOfImplementation();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("extra-implementation"))) {
					i = ReadExtraImplementation();
					if (i <= 0) return i;
					continue;
				} if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("bridge"))) {
					i = ReadBridge();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("implementation"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadDefinitionOfImplementation() {
		std::unique_ptr<Definition> definition(new Definition);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				definition->set_type(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("sub-type"))) {
				definition->set_sub_type(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("format"))) {
				definition->set_format(xmlTextReaderValue(text_reader_));
			}
		}
		if (xmlStrEqual(definition->type(), reinterpret_cast<const xmlChar *>("assign"))) {
			if (xmlStrEqual(definition->sub_type(), reinterpret_cast<const xmlChar *>("bridge"))) {
				return xmlTextReaderNext(text_reader_);
			}
			if (xmlStrEqual(definition->format(), reinterpret_cast<const xmlChar *>("reference"))) {
				// expect assigning a port or timeseries
				i = xmlTextReaderRead(text_reader_);
				while (i > 0) {
					int type = xmlTextReaderNodeType(text_reader_);
					if (type == XML_READER_TYPE_ELEMENT) {
						const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
						if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("reference"))) {
							i = ReadReference();
							if (i <= 0) return i;
							continue;
						}
					} else if (type == XML_READER_TYPE_END_ELEMENT) {
						const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
						if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("definition"))) {
							return xmlTextReaderRead(text_reader_);
						}
					}
					i = xmlTextReaderRead(text_reader_);
				}
				return i;
			}
		} else if (xmlStrEqual(definition->type(), reinterpret_cast<const xmlChar *>("graph"))) {
			pq_->set_unit_id(xmlCharStrdup("0")); // unit: dimensionless
			GraphReader graph_reader(pq_.get(), text_reader_, dd_.get());
			return graph_reader.Read();
		} else if (xmlStrEqual(definition->type(), reinterpret_cast<const xmlChar *>("predefined"))) {
			if (xmlStrEqual(definition->sub_type(), reinterpret_cast<const xmlChar *>("Wiener-process"))) {
				is_Wiener_process_ = true;
				return 1;
			}
			std::cerr << "unknown sub-type of predefined <implementation>: "
					  << definition->sub_type()
					  << std::endl;
			return -2;
		}
		// expect definition in MathML
		impl_dumper_.reset(new DefinitionDumper<Implementation>(text_reader_,
																	  pq_->name(),
																	  impl_.get()));
		return impl_dumper_->Read(0);
	}

	int ReadReference() {
		ref_.reset(new Reference);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("port-id"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				int port_id = std::atoi(reinterpret_cast<const char *>(value));
				if (port_id <= 0) {
					std::cerr << "invalid port-id of <reference>: " << value << std::endl;
					return -2;
				}
				ref_->set_port_id(port_id);
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("timeseries-id"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				int timeseries_id = std::atoi(reinterpret_cast<const char *>(value));
				if (timeseries_id <= 0) {
					std::cerr << "invalid timeseries-id of <reference>: " << value << std::endl;
					return -2;
				}
				ref_->set_timeseries_id(timeseries_id);
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("element-id"))) {
				ref_->set_element_id(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <reference>: " << local_name << std::endl;
				return -2;
			}
		}
		return xmlTextReaderNext(text_reader_);
	}

	int ReadExtraImplementation() {
		extra_.reset(new ExtraImplementation);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				extra_->set_type(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("order"))) {
				xmlChar *value = xmlTextReaderValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("before"))) {
					extra_->set_order(value);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("after"))) {
					extra_->set_order(value);
				} else if (xmlStrlen(value) == 0) {
					xmlFree(value);
					/* ignore this with warning */
					std::cerr << "warning: empty value of order of <extra-implementation> of "
						 << pq_->name()
						 << " in "
						 << module_->module_id()
						 << std::endl;
				} else {
					std::cerr << "unknown value of order: " << value << std::endl;
					xmlFree(value);
					return -2;
				}
			} else {
				std::cerr << "unknown attribute of <extra-implementation>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!extra_->type()) {
			std::cerr << "missing type of <extra-implementation>" << std::endl;
			return -2;
		}
		if ( !xmlStrEqual(extra_->type(), reinterpret_cast<const xmlChar *>("instantaneous")) &&
			 !xmlStrEqual(extra_->type(), reinterpret_cast<const xmlChar *>("multiple-input-assignment")) ) {
			// TODO: other types of <extra-implementation>
			std::cerr << "unsupported type of <extra-implementation>: "
				 << extra_->type()
				 << std::endl;
			return -2;
		}

		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("definition"))) {
					i = ReadDefinitionOfExtraImplementation();
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child element of <extra-implementation>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("extra-implementation"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadDefinitionOfExtraImplementation() {
		std::unique_ptr<Definition> def(new Definition);
		int isEmpty = xmlTextReaderIsEmptyElement(text_reader_);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				def->set_type(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("sub-type"))) {
				def->set_sub_type(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("format"))) {
				def->set_format(xmlTextReaderValue(text_reader_));
			}
		}
		if (xmlStrEqual(extra_->type(), reinterpret_cast<const xmlChar *>("multiple-input-assignment"))) {
			if (xmlStrEqual(def->type(), reinterpret_cast<const xmlChar *>("loop"))) {
				std::cerr << "multiple-input-assignment's definition of type loop is not supported yet" << std::endl;
				return -2;
			}
			if (!xmlStrEqual(def->type(), reinterpret_cast<const xmlChar *>("reduction"))) {
				std::cerr << "found <extra-implementation> of type multiple-input-assignment, but its definition's type is not reduction: "
					 << def->type() << std::endl;
				return -2;
			}
			extra_->set_definition(def.release());
			return xmlTextReaderNext(text_reader_);
		}
		assert(xmlStrEqual(extra_->type(), reinterpret_cast<const xmlChar *>("instantaneous")));
		if (isEmpty != 0) {
			std::cerr << "empty <definition> in <extra-implementation> of type instantaneous" << std::endl;
			return -2;
		}
		if (!extra_->order()) {
			std::cerr << "missing order of <extra-implementation>" << std::endl;
			return -2;
		}
		// expect definition in MathML
		extra_dumper_.reset(new DefinitionDumper<ExtraImplementation>(text_reader_,
																			pq_->name(),
																			extra_.get()));
		return extra_dumper_->Read(0);
	}

	int ReadBridge() {
		bridge_.reset(new Bridge);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (!xmlStrEqual(value, reinterpret_cast<const xmlChar *>("sbml"))) {
					std::cerr << "unknown type of <bridge>: " << value << std::endl;
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("sub-type"))) {
				xmlChar *value = xmlTextReaderValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("species"))) {
					bridge_->set_sub_type(value);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("parameter"))) {
					bridge_->set_sub_type(value);
				} else {
					std::cerr << "unknown sub-type of <bridge>: " << value << std::endl;
					xmlFree(value);
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("direction"))) {
				xmlChar *value = xmlTextReaderValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("get"))) {
					bridge_->set_direction(value);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("set"))) {
					bridge_->set_direction(value);
				} else {
					std::cerr << "unknown direction of <bridge>: " << value << std::endl;
					xmlFree(value);
					return -2;
				}
			} else {
				std::cerr << "unknown attribute of <bridge>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!bridge_->sub_type()) {
			std::cerr << "missing sub-type of <bridge>" << std::endl;
			return -2;
		}
		if (!bridge_->direction()) {
			std::cerr << "missing direction of <bridge>" << std::endl;
			return -2;
		}

		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("connector"))) {
					i = ReadConnector();
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child element of <bridge>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("bridge"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadConnector() {
		assert(bridge_);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("parameter"))) {
					if (!xmlStrEqual(bridge_->sub_type(), reinterpret_cast<const xmlChar *>("parameter"))) {
						std::cerr << "mismatch between bridge's sub-type and connector's type" << std::endl;
						return -2;
					}
					continue;
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("species"))) {
					if (!xmlStrEqual(bridge_->sub_type(), reinterpret_cast<const xmlChar *>("species"))) {
						std::cerr << "mismatch between bridge's sub-type and connector's type" << std::endl;
						return -2;
					}
					continue;
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("reaction"))) {
					// skip this connector
					return xmlTextReaderNext(text_reader_);
				} else {
					std::cerr << "unknown type of <connector>: " << local_name << std::endl;
					return -2;
				}
			} else {
				std::cerr << "unknown attribute of <connector>: " << local_name << std::endl;
				return -2;
			}
		}
		if (i < 0) return i;

		i = xmlTextReaderRead(text_reader_);
		if (i <= 0) return i;
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		if (!s) {
			std::cerr << "missing body of <connector>" << std::endl;
			return -2;
		}
		bridge_->set_connector(s);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadImport() const {
		std::unique_ptr<Import> import(new Import);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				xmlChar *value = xmlTextReaderValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("external"))) {
					import->set_type(value);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("internal"))) {
					import->set_type(value);
				} else {
					std::cerr << "unknown type of <import>: " << value << std::endl;
					xmlFree(value);
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("format"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("sbml"))) {
					import->set_format(Import::kSbml);
				} else {
					std::cerr << "unknown format of <import>: " << value << std::endl;
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("iref"))) {
				import->set_iref(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("xref"))) {
				import->set_xref(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("zref"))) {
				import->set_zref(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <import>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!import->type()) {
			std::cerr << "missing type of <import>" << std::endl;
			return -2;
		}
		if (import->format() == Import::kUnspecifiedFormat) {
			std::cerr << "missing format of <import>" << std::endl;
			return -2;
		}
		if (!dd_->SaveImport(module_.get(), import.get(), given_path_, model_path_)) return -2;
		return xmlTextReaderNext(text_reader_);
	}

	int ReadTimeseries() {
		std::unique_ptr<Timeseries> ts(new Timeseries);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("file"))) {
					ts->set_type(Timeseries::Type::kFile);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("ipc"))) {
					ts->set_type(Timeseries::Type::kIpc);
				} else {
					std::cerr << "unknown type of <timeseries>: " << value << std::endl;
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("timeseries-id"))) {
				ts->set_timeseries_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("name"))) {
				// we can ignore the name
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("format"))) {
				xmlChar *value = xmlTextReaderValue(text_reader_);
				if ( xmlStrEqual(value, reinterpret_cast<const xmlChar *>("csv")) ||
					 xmlStrEqual(value, reinterpret_cast<const xmlChar *>("isd")) ) {
					ts->set_format(value);
				} else {
					std::cerr << "unknown format of <timeseries>: " << value << std::endl;
					xmlFree(value);
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("iref"))) {
				ts->set_iref(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("zref"))) {
				ts->set_zref(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("url"))) {
				ts->set_url(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <timeseries>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!ts->timeseries_id()) {
			std::cerr << "missing timeseries-id of <timeseries>" << std::endl;
			return -2;
		}
		if (ts->type() == Timeseries::Type::kUnspecified) {
			// for backward compatibility
			ts->set_type(Timeseries::Type::kFile);
		}
		switch (ts->type()) {
		case Timeseries::Type::kUnspecified:
			assert(false);
			break;
		case Timeseries::Type::kFile:
			if (!ts->format()) {
				std::cerr << "missing format of <timeseries>" << std::endl;
				return -2;
			}
			if (!ts->iref() && !ts->zref()) {
				std::cerr << "missing iref/zref of <timeseries>" << std::endl;
				return -2;
			}
			if (!dd_->SaveTimeseries(module_.get(), ts.get(), given_path_, model_path_))
				return -2;
			break;
		case Timeseries::Type::kIpc:
			if (!ts->url()) {
				std::cerr << "missing url of <timeseries>" << std::endl;
				return -2;
			}
			if (!dd_->SaveTsipc(*module_, *ts))
				return -2;
			break;
		}
		return xmlTextReaderNext(text_reader_);
	}

	int ReadTemplateSet() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) {
			// no templates, so we have done
			return xmlTextReaderNext(text_reader_);
		}
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("template"))) {
					i = ReadTemplate();
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <template-set>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("template-set"))) {
					return xmlTextReaderNext(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadTemplate() {
		int i;
		std::unique_ptr<Template> t(new Template);
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("template-id"))) {
				t->set_template_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("ref-module-id"))) {
				t->set_ref_module_id(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <template>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!t->template_id()) {
			std::cerr << "missing template-id of <template>" << std::endl;
			return -2;
		}
		if (!t->ref_module_id()) {
			std::cerr << "missing ref-module-id of <template>: " << t->template_id() << std::endl;
			return -2;
		}
		if (!dd_->SaveTemplate(t.get())) return -2;
		return xmlTextReaderNext(text_reader_);
	}

	int ReadInstanceSet() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) {
			// no instances, so we have done
			return xmlTextReaderRead(text_reader_);
		}
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("instance"))) {
					i = ReadInstance();
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <instance-set>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("instance-set"))) {
					return xmlTextReaderNext(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadInstance() {
		instance_.reset(new Instance);

		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("module-id"))) {
				instance_->set_module_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("label"))) {
				instance_->set_label(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <instance>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!instance_->module_id()) {
			std::cerr << "missing module-id of <instance>" << std::endl;
			return -2;
		}
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("instance-of"))) {
					i = ReadInstanceOf();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("target-module"))) {
					i = ReadTargetModule();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("instance"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadInstanceOf() {
		assert(instance_);

		xmlChar *template_id;
		int i = ReadAttributeValue(reinterpret_cast<const xmlChar *>("template-id"), &template_id);
		if (i <= 0) {
			std::cerr << "missing template-id of <instance-of>: " << instance_->module_id() << std::endl;
			return -2;
		}
		instance_->set_template_id(template_id);
		if (!dd_->SaveInstance(instance_.get())) return -2;
		return xmlTextReaderNext(text_reader_);
	}

	int ReadTargetModule() {
		xmlChar *module_id;
		int i = ReadAttributeValue(reinterpret_cast<const xmlChar *>("module-id"), &module_id);
		if (i <= 0) {
			std::cerr << "missing module-id of <target-module>: " << instance_->module_id() << std::endl;
			return -2;
		}
		std::unique_ptr<TargetModule> tm(new TargetModule(module_id));
		if (!dd_->SaveTargetModule(instance_.get(), tm.get())) return -2;
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("target-physical-quantity"))) {
					i = ReadTargetPq(tm.get());
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("target-module"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return xmlTextReaderNext(text_reader_);
	}

	int ReadTargetPq(const TargetModule *tm) {
		assert(tm);

		int pq_id = 0;
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("physical-quantity-id"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				pq_id = std::atoi(reinterpret_cast<const char *>(value));
				if (pq_id <= 0) {
					std::cerr << "invalid physical-quantity-id of <target-physical-quantity>: " << tm->module_id() << std::endl;
					return -2;
				}
			}
		}
		if (pq_id <= 0) {
			std::cerr << "missing physical-quantity-id of <target-physical-quantity>: " << tm->module_id() << std::endl;
			return -2;
		}
		std::unique_ptr<TargetPq> tpq(new TargetPq(pq_id));
		std::unique_ptr<DefinitionDumper<TargetPq> > tpq_dumper(new DefinitionDumper<TargetPq>(text_reader_, tpq.get()));
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("definition"))) {
					i = tpq_dumper->Read(0);
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("target-physical-quantity"))) {
					if (!dd_->SaveTargetPq(tm, tpq.get())) return -2;
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadEdgeSet() {
		if (xmlTextReaderIsEmptyElement(text_reader_)) {
			// no edges, so we have done
			return xmlTextReaderRead(text_reader_);
		}
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("edge"))) {
					i = ReadEdge();
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <edge-set>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("edge-set"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadEdge() {
		edge_.reset();
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("edge-id"))) {
				// ignored
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("functional"))) {
					edge_.reset(new Edge(Edge::kFunctional));
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("forwarding"))) {
					edge_.reset(new Edge(Edge::kForwarding));
				} else {
					// let's skip this edge
					return xmlTextReaderNext(text_reader_);
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("multiple"))) {
				// ignored
			} else {
				std::cerr << "unknown attribute of <edge>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!edge_) {
			std::cerr << "missing type of <edge>" << std::endl;
			return -2;
		}
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("tail"))) {
					i = ReadTail();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("head"))) {
					i = ReadHead();
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("edge"))) {
					if (!dd_->SaveEdge(edge_.get())) return -2;
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadTail() {
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("module-id"))) {
				edge_->set_tail_module_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("port-id"))) {
				edge_->set_tail_port_id(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <tail>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!edge_->tail_module_id()) {
			std::cerr << "missing module-id of <tail>" << std::endl;
			return -2;
		}
		if (!edge_->tail_port_id()) {
			std::cerr << "missing port-id of <tail>" << std::endl;
			return -2;
		}
		return xmlTextReaderNext(text_reader_);
	}

	int ReadHead() {
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("module-id"))) {
				edge_->set_head_module_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("port-id"))) {
				edge_->set_head_port_id(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <head>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!edge_->head_module_id()) {
			std::cerr << "missing module-id of <head>" << std::endl;
			return -2;
		}
		if (!edge_->head_port_id()) {
			std::cerr << "missing port-id of <head>" << std::endl;
			return -2;
		}
		return xmlTextReaderNext(text_reader_);
	}

	int ReadAttributeValue(const xmlChar *name, xmlChar **value)
	{
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, name)) {
				*value = xmlTextReaderValue(text_reader_);
				return i;
			}
		}
		return i;
	}

	const boost::filesystem::path &given_path_;
	const boost::filesystem::path &model_path_;
	xmlTextReaderPtr &text_reader_;
	std::unique_ptr<DatabaseDriver> dd_;

	std::unique_ptr<Module> module_;
	std::unique_ptr<PQ> pq_;
	std::unique_ptr<InitialValue> iv_;
	std::unique_ptr<Implementation> impl_;
	std::unique_ptr<Reference> ref_;
	std::unique_ptr<ExtraImplementation> extra_;
	std::unique_ptr<Edge> edge_;
	std::unique_ptr<Bridge> bridge_;
	std::unique_ptr<Instance> instance_;

	std::unique_ptr<DefinitionDumper<InitialValue> > iv_dumper_;
	std::unique_ptr<DefinitionDumper<Implementation> > impl_dumper_;
	std::unique_ptr<DefinitionDumper<ExtraImplementation> > extra_dumper_;
	bool is_Wiener_process_;
};

struct Schema {
	const char *name;
	const char *columns;
};

const Schema kModelTables[] = {
	{"edges", "(tail_module_id BLOB, tail_port_id INTEGER, head_module_id BLOB, head_port_id INTEGER)"},
	{"instances", "(module_id BLOB, template_id BLOB, label TEXT)"},
	{"ncs", "(rg_name TEXT, rg_seed TEXT, integration TEXT, sts_unit_id INTEGER, sts_value TEXT)"},
	{"tds", "(unit_id INTEGER, step TEXT, module_id BLOB)"},
	{"modules", "(module_id BLOB, type TEXT, name TEXT, capsulated_by BLOB, template_state TEXT)"},
	{"pqs", "(module_rowid INTEGER, type TEXT, pq_id INTEGER, unit_id INTEGER, name TEXT, ncols INTEGER, nrows INTEGER, max_delay TEXT, independent INTEGER)"},
	{"ivs", "(pq_rowid INTEGER, math TEXT)"},
	{"impls", "(pq_rowid INTEGER, math TEXT)"},
	{"ecs", "(pq_rowid INTEGER, math TEXT)"},
	{"nodes", "(pq_rowid INTEGER, node_id INTEGER, name TEXT)"},
	{"arcs", "(pq_rowid INTEGER, tail_node_id INTEGER, head_node_id INTEGER, type TEXT, math TEXT)"},
	{"refports", "(pq_rowid INTEGER, port_id INTEGER, reduction INTEGER)"},
	{"refts", "(pq_rowid INTEGER, timeseries_id INTEGER, element_id TEXT)"},
	{"extras", "(pq_rowid INTEGER, order_type TEXT, math TEXT)"},
	{"templates", "(template_id BLOB, ref_module_id BLOB)"},
	{"tms", "(instance_rowid INTEGER, module_id BLOB)"},
	{"tpqs", "(tm_rowid INTEGER, pq_id INTEGER, math TEXT)"},
	{"units", "(unit_id INTEGER, name TEXT)"},
	{"elements", "(unit_rowid INTEGER, unit_id INTEGER, exponent REAL, factor INTEGER, multiplier REAL, offset REAL)"},
	{"bridges", "(pq_rowid INTEGER, direction TEXT, sub_type TEXT, connector TEXT)"},
	{"imports", "(module_rowid INTEGER, type TEXT, ref TEXT)"},
	{"ports", "(module_rowid INTEGER, port_id INTEGER, direction TEXT, ref_pq_id INTEGER, multiple TEXT)"},
	{"timeseries", "(module_rowid INTEGER, timeseries_id INTEGER, format TEXT, ref TEXT)"},
	{"tsipc", "(module_rowid INTEGER, timeseries_id INTEGER, url TEXT)"}
};

const Schema kSubsequentTables[] = {
	{"private_variables", VARIABLES_SCHEMA}, // the same columns as public_variables
	{"trees", "(module_id BLOB, level INTEGER)"},
	{"scopes", "(uuid BLOB, space_id BLOB, label TEXT)"},
	{"journals", "(indent INTEGER, uuid BLOB)"},
	{"spans", "(tail_uuid BLOB, tail_port_id INTEGER, head_uuid BLOB, head_port_id INTEGER)"},
	{"reaches", REACHES_SCHEMA},
	{"combined_values", "(uuid BLOB, math TEXT)"},
	{"combined_functions", "(uuid BLOB, math TEXT)"},
	{"combined_odes", "(uuid BLOB, math TEXT)"},
	{"tscs", "(uuid BLOB, math TEXT)"},
	{"tsipcforms", "(uuid BLOB, math TEXT)"}
};

bool CreateTables(sqlite3 *db, const Schema *tables, size_t n)
{
	for (size_t i=0;i<n;i++) {
		const Schema &table = tables[i];
		if (!CreateTable(db, table.name, table.columns))
			return false;
	}
	return true;
}

#define CREATE_TABLES(db, tables) \
	CreateTables(db, tables, sizeof(tables)/sizeof(tables[0]))

struct View {
	const char *name;
	const char *rest_of_query;
};

const View kViews[] = {
	{"joins", "m.module_id AS module_id, i.module_id AS uuid, i.label AS label FROM instances AS i LEFT JOIN templates As t ON i.template_id = t.template_id LEFT JOIN modules AS m ON m.module_id = t.ref_module_id"},
	{"spaces", "module_id AS space_id, name FROM modules WHERE type = 'functional-unit'"},
	{"public_variables", "m.module_id AS space_id, p.type, p.pq_id AS id, p.name, u.name AS unit, p.ncols, p.nrows, p.max_delay AS capacity, p.independent FROM pqs AS p LEFT JOIN modules AS m ON p.module_rowid = m.rowid LEFT JOIN units AS u ON p.unit_id = u.unit_id"},
	{"variables", "* FROM public_variables UNION ALL SELECT * FROM private_variables"},
	{"time_unit", "u.name FROM tds AS t JOIN units AS u ON t.unit_id = u.unit_id WHERE t.module_id IS NULL"},
	{"sv_eqs", "m.module_id, ltrim(i.math) FROM impls AS i LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid WHERE p.type = 's' OR p.type = 'v'"},
	{"vx_eqs", "m.module_id, ltrim(i.math) FROM impls AS i LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid WHERE p.type = 'v' OR p.type = 'x'"},
	{"iv_eqs", "m.module_id, ltrim(i.math) FROM ivs AS i LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid"},
	{"input_ivs", "* FROM sv_eqs UNION ALL SELECT * FROM iv_eqs UNION ALL SELECT * FROM tscs UNION ALL SELECT * FROM tsipcforms UNION ALL SELECT * FROM combined_values UNION ALL SELECT * FROM combined_functions"},
	{"input_eqs", "* FROM vx_eqs UNION ALL SELECT * FROM tscs UNION ALL SELECT * FROM tsipcforms UNION ALL SELECT * FROM combined_functions UNION ALL SELECT * FROM combined_odes"},
	{"si_eqs", "m.module_id, ltrim(i.math) FROM impls AS i LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid WHERE p.type = 's' AND p.independent = '1'"},
	{"ivi_eqs", "m.module_id, ltrim(i.math) FROM ivs AS i LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid WHERE p.independent = '1'"},
	{"independent_ivs", "* FROM si_eqs UNION ALL SELECT * FROM ivi_eqs"},
	{"dependent_ivs", "* FROM input_ivs EXCEPT SELECT * FROM independent_ivs"},
	{"before_eqs", "m.module_id, ltrim(e.math) FROM extras AS e LEFT JOIN pqs AS p ON e.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid WHERE e.order_type = 'before'"},
	{"after_eqs", "m.module_id, ltrim(e.math) FROM extras AS e LEFT JOIN pqs AS p ON e.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid WHERE e.order_type = 'after'"}
};

bool CreateViews(sqlite3 *db, const View *views, size_t n)
{
	char buf[1024]; // long enough
	char *em;
	int e;

	for (size_t i=0;i<n;i++) {
		const View &view = views[i];
		sprintf(buf, "CREATE VIEW IF NOT EXISTS %s AS SELECT %s",
				view.name, view.rest_of_query);

		e = sqlite3_exec(db, buf, nullptr, nullptr, &em);
		if (e != SQLITE_OK) {
			std::cerr << "failed to create view " << view.name
				 << ": " << e
				 << ": " << em << std::endl;
			sqlite3_free(em);
			return false;
		}
	}
	return true;
}

#define CREATE_VIEWS(db, views) \
	CreateViews(db, views, sizeof(views)/sizeof(views[0]))

}

bool Read(sqlite3 *db, const boost::filesystem::path &dir)
{
	std::unique_ptr<char[]> given_filename(GetGivenFilename(db));
	if (!given_filename)
		return false;
	boost::filesystem::path given_path(given_filename.get());
	std::unique_ptr<char[]> model_filename(GetModelFilename(db));
	if (!model_filename)
		return false;
	boost::filesystem::path model_path(model_filename.get());

	// prepare database; create tables
	if (!BeginTransaction(db))
		return false;
	if (!CREATE_TABLES(db, kModelTables))
		return false;

	// subsequent tables
	if (!CREATE_TABLES(db, kSubsequentTables))
		return false;
	if (!CreateSprinkles(db))
		return false;
	if (!CreateTsfiles(db))
		return false;
	if (!CreateChannels(db))
		return false;
	if (!CreateConfig(db))
		return false;

	// views
	if (!CREATE_VIEWS(db, kViews))
		return false;
	if (!CreateLayout(db))
		return false;

	LIBXML_TEST_VERSION
	xmlInitParser();

	xmlTextReaderPtr text_reader = xmlReaderForFile(model_filename.get(), nullptr, 0);
	if (!text_reader) {
		std::cerr << "could not read the input: " << model_filename.get() << std::endl;
		xmlCleanupParser();
		return false;
	}

	{
		std::unique_ptr<Reader> reader(new Reader(given_path, model_path, text_reader, db));
		if (reader->Read() < 0) return false;
	}

	// validate the model immediately after reading it
	{
		VariableDefinitionValidator vdv(db);
		if (!vdv.Validate())
			return false;
	}
	{
		std::unique_ptr<CapsulatedByValidator> validator(new CapsulatedByValidator);
		if (!validator->Validate(db))
			return false;
	}
	{
		DelayArgValidator dav(db);
		if (!dav.Validate()) return false;
	}

	// then, rewrite the model
	{
		std::unique_ptr<GraphIvRewriter> rewriter(new GraphIvRewriter);
		if (!rewriter->Rewrite(db))
			return false;
	}
	{
		static const char kImplSelectQuery[] = "SELECT rowid, pq_rowid, math FROM impls";
		static const char kImplUpdateQuery[] = "UPDATE impls SET math = ? WHERE rowid = ?";
		GraphMathRewriter rewriter(kImplSelectQuery, kImplUpdateQuery, db);
		if (!rewriter.Rewrite())
			return false;
	}
	{
		static const char kExtraSelectQuery[] = "SELECT rowid, pq_rowid, math FROM extras";
		static const char kExtraUpdateQuery[] = "UPDATE extras SET math = ? WHERE rowid = ?";
		GraphMathRewriter rewriter(kExtraSelectQuery, kExtraUpdateQuery, db);
		if (!rewriter.Rewrite())
			return false;
	}
	{
		static const char kTpqSelectQuery[] = \
			"SELECT tpqs.rowid, pqs.rowid, tpqs.math FROM tpqs"
			" LEFT JOIN tms ON tpqs.tm_rowid = tms.rowid"
			" LEFT JOIN modules ON tms.module_id = modules.module_id"
			" LEFT JOIN pqs ON modules.rowid = pqs.module_rowid"
			" WHERE tpqs.pq_id = pqs.pq_id";
		static const char kTpqUpdateQuery[] = "UPDATE tpqs SET math = ? WHERE rowid = ?";
		GraphMathRewriter rewriter(kTpqSelectQuery, kTpqUpdateQuery, db);
		if (!rewriter.Rewrite())
			return false;
	}
	{
		TransitionForm form;
		if (!form(db))
			return false;
	}

	{
		std::unique_ptr<TreeWriter> tw(new TreeWriter);
		if (!tw->Write(db))
			return false;
	}

	if (!Branch(model_path, db)) return false;
	if (!Span(db)) return false;
	if (!Reach(db)) return false;
	if (!Sprinkle(db)) return false;

	if (!CommitTransaction(db))
		return false;

	if (!CombineAll(db, dir))
		return false;
	if (!ts::Tsc(db))
		return false;
	if (!Tsipc(db))
		return false;
	if (!db::Flow(db))
		return false;
	return true;
}

}
}
