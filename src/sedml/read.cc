/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "sedml.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include <sedml/reader.h>

#include "db/query.h"
#include "sqlite3.h"
#include "utf8path.h"

using std::cerr;
using std::endl;

namespace flint {
namespace sedml {

namespace {

enum {
	kEuler,
	kRungeKutta4th,
	kArk
};

const char *kAlgorithmName[] = {
	"euler",
	"rk4",
	"ark"
};

int BindAlgorithm(const struct sedml_uniformtimecourse *utc,
						 sqlite3_stmt *stmt)
{
	if (!utc->algorithm || !utc->algorithm->kisaoID) {
		fprintf(stderr, "invalid algorithm in SED-ML\n");
		return 0;
	}
	int i;
	const char *kisaoID = utc->algorithm->kisaoID;
	if (strcmp(kisaoID, "KISAO:0000030") == 0) {
		i = kEuler;
	} else if (strcmp(kisaoID, "KISAO:0000032") == 0) {
		i = kRungeKutta4th;
	} else if (strcmp(kisaoID, "KISAO:9999999") == 0) {
		i = kArk;
	} else {
		fprintf(stderr, "unexpected kisaoID of algorithm in SED-ML: %s\n",
				kisaoID);
		return 0;
	}
	int e = sqlite3_bind_text(stmt, 1, kAlgorithmName[i], -1, SQLITE_STATIC);
	return e == SQLITE_OK;
}

int BindLength(const struct sedml_uniformtimecourse *utc,
					  sqlite3_stmt *stmt)
{
	int e = sqlite3_bind_double(stmt, 2, utc->outputEndTime);
	return e == SQLITE_OK;
}

int BindStep(const struct sedml_uniformtimecourse *utc,
					sqlite3_stmt *stmt)
{
	int e = sqlite3_bind_double(stmt, 3, utc->outputEndTime/utc->numberOfPoints);
	return e == SQLITE_OK;
}

int BindGranularity(const struct sedml_uniformtimecourse *utc,
						   sqlite3_stmt *stmt)
{
	int e;
	if (utc->num_xml_attributes > 0 && utc->xml_attributes) {
		for (int i=0;i<utc->num_xml_attributes;i++) {
			struct sedml_xml_attribute *attr = utc->xml_attributes[i];
			if (!attr || !attr->local_name) continue;
			if (strcmp(attr->local_name, "granularity") == 0) {
				if (!attr->value) continue;
				int n = atoi(attr->value);
				if (n <= 0) continue;
				e = sqlite3_bind_int(stmt, 4, n);
				return e == SQLITE_OK;
			}
		}
	}
	/* default */
	e = sqlite3_bind_int(stmt, 4, 1);
	return e == SQLITE_OK;
}

bool BindOutputStartTime(const struct sedml_uniformtimecourse *utc,
						 sqlite3_stmt *stmt)
{
	int e = sqlite3_bind_double(stmt, 5, utc->outputStartTime);
	if (e != SQLITE_OK) {
		cerr << "failed to bind output_start_time: " << e << endl;
		return false;
	}
	return true;
}

}

bool Read(const char *sedml_file, sqlite3 *db)
{
	bool r = false;
	boost::filesystem::path sedml_path = GetPathFromUtf8(sedml_file);
	std::string sedml_path_s = sedml_path.string();

	struct sedml_document *doc = sedml_create_document();
	const struct sedml_sedml *sedml;
	if (!doc) {
		fprintf(stderr, "failed to allocate SED-ML document\n");
		return false;
	}
	if (sedml_read_file(sedml_path_s.c_str(), NULL, doc) < 0) {
		fprintf(stderr, "failed to read SED-ML file: %s\n", sedml_file);
		goto bail;
	}
	sedml = doc->sedml;
	if (!sedml) {
		fprintf(stderr, "no <sedML> element in SED-ML\n");
		goto bail;
	}
	if (sedml->num_models <=0 || !sedml->models) {
		fprintf(stderr, "no <model>s in SED-ML\n");
		goto bail;
	}
	if (sedml->num_simulations <= 0 || !sedml->simulations) {
		fprintf(stderr, "no simulations in SED-ML\n");
		goto bail;
	}
	if (sedml->num_tasks <= 0 || !sedml->tasks) {
		fprintf(stderr, "no tasks in SED-ML\n");
		goto bail;
	}

	if (!BeginTransaction(db))
		goto bail;
	if (!CreateTable(db, "tasks", "(model_id INTEGER, sim_id INTEGER)"))
		goto bail;
	if (!CreateTable(db, "models", "(model_path TEXT, absolute_path TEXT)"))
		goto bail;
	if (!CreateTable(db, "sims", "(algorithm TEXT, length REAL, step REAL, granularity INTEGER, output_start_time REAL)"))
		goto bail;
	if (!CreateTable(db, "dgs", "(task_id INTEGER, variable TEXT)"))
		goto bail;

	for (int i=0;i<sedml->num_tasks;i++) {
		const struct sedml_task *task = sedml->tasks[i];
		if (!task || !task->modelReference || !task->simulationReference) {
			fprintf(stderr, "invalid task in SED-ML\n");
			goto bail;
		}

		const struct sedml_model *model = NULL;
		for (int k=0;k<sedml->num_models;k++) {
			const struct sedml_model *m = sedml->models[k];
			if (!m || !m->id) continue;
			if (strcmp(m->id, task->modelReference) == 0) {
				model = m;
				break;
			}
		}
		if (!model) {
			fprintf(stderr, "invalid model reference in SED-ML: %s\n",
					task->modelReference);
			goto bail;
		}

		sqlite3_stmt *stmt;
		int e;
		e = sqlite3_prepare_v2(db, "INSERT INTO models VALUES (?, NULL)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to prepare statement: %d\n", e);
			goto bail;
		}
		assert(model);
		e = sqlite3_bind_text(stmt, 1, model->source, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to bind parameter: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to step statement: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		sqlite3_finalize(stmt);

		sqlite3_int64 model_id = sqlite3_last_insert_rowid(db);

		const struct sedml_simulation *simulation = NULL;
		for (int i=0;i<sedml->num_simulations;i++) {
			const struct sedml_simulation *s = sedml->simulations[i];
			if (!s || !s->id) continue;
			if (strcmp(s->id, task->simulationReference) == 0) {
				simulation = s;
				break;
			}
		}
		if (!simulation) {
			fprintf(stderr, "invalid simulation reference in SED-ML\n");
			goto bail;
		}
		assert(simulation);
		if (simulation->simulation_type != SEDML_UNIFORM_TIME_COURSE) {
			fprintf(stderr,
					"simulation other than uniform time course in SED-ML\n");
			goto bail;
		}

		const struct sedml_uniformtimecourse *utc;
		utc = (const struct sedml_uniformtimecourse *)simulation;

		e = sqlite3_prepare_v2(db, "INSERT INTO sims VALUES (?, ?, ?, ?, ?)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to prepare statement: %d\n", e);
			goto bail;
		}
		if (!BindAlgorithm(utc, stmt)) {
			fprintf(stderr, "failed to bind algorithm: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		if (!BindLength(utc, stmt)) {
			fprintf(stderr, "failed to bind length: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		if (!BindStep(utc, stmt)) {
			fprintf(stderr, "failed to bind step: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		if (!BindGranularity(utc, stmt)) {
			fprintf(stderr, "failed to bind granularity: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		if (!BindOutputStartTime(utc, stmt))
			goto bail;
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to step statement: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		sqlite3_finalize(stmt);

		sqlite3_int64 sim_id = sqlite3_last_insert_rowid(db);

		e = sqlite3_prepare_v2(db, "INSERT INTO tasks VALUES (?, ?)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			goto bail;
		}
		e = sqlite3_bind_int64(stmt, 1, model_id);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to bind model_id: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		e = sqlite3_bind_int64(stmt, 2, sim_id);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to bind sim_id: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to step statement: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail;
		}
		sqlite3_finalize(stmt);

		sqlite3_int64 task_id = sqlite3_last_insert_rowid(db);

		if (doc->sedml->num_datagenerators > 0 && doc->sedml->datagenerators) {
			struct sedml_datagenerator *datagenerator;
			struct sedml_variable *variable;
			for (int i=0;i<doc->sedml->num_datagenerators;i++) {
				datagenerator = doc->sedml->datagenerators[i];
				if (datagenerator->num_variables != 1) continue;
				if (!datagenerator->variables) continue;
				variable = datagenerator->variables[0];
				if (!variable) continue;
				if (!variable->taskReference) continue;
				if (strcmp(task->id, variable->taskReference) != 0) continue;
				if (!variable->target) continue;

				e = sqlite3_prepare_v2(db, "INSERT INTO dgs VALUES (?, ?)",
									   -1, &stmt, NULL);
				if (e != SQLITE_OK) {
					cerr << "failed to prepare statement: " << e << endl;
					goto bail;
				}
				e = sqlite3_bind_int64(stmt, 1, task_id);
				if (e != SQLITE_OK) {
					fprintf(stderr, "failed to bind task_id: %d\n", e);
					sqlite3_finalize(stmt);
					goto bail;
				}
				e = sqlite3_bind_text(stmt, 2, variable->target, -1, SQLITE_STATIC);
				if (e != SQLITE_OK) {
					fprintf(stderr, "failed to bind variable: %d\n", e);
					sqlite3_finalize(stmt);
					goto bail;
				}
				e = sqlite3_step(stmt);
				if (e != SQLITE_DONE) {
					fprintf(stderr, "failed to step statement: %d\n", e);
					sqlite3_finalize(stmt);
					goto bail;
				}
				sqlite3_finalize(stmt);
			}
		}
	}

	if (!CommitTransaction(db))
		goto bail;
	r = true;

 bail:
	sedml_destroy_document(doc);
	return r;
}

}
}
