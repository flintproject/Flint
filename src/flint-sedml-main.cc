/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>

#include <sedml/reader.h>

#include "database.h"
#include "db/query.h"
#include "sqlite3.h"
#include "utf8path.h"

enum {
	kEuler,
	kRungeKutta4th
};

static const char *kAlgorithmName[] = {
	"euler",
	"rk4"
};

static int BindAlgorithm(const struct sedml_uniformtimecourse *utc,
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
	} else {
		fprintf(stderr, "unexpected kisaoID of algorithm in SED-ML: %s\n",
				kisaoID);
		return 0;
	}
	int e = sqlite3_bind_text(stmt, 1, kAlgorithmName[i], -1, SQLITE_STATIC);
	return e == SQLITE_OK;
}

static int BindLength(const struct sedml_uniformtimecourse *utc,
					  sqlite3_stmt *stmt)
{
	int e = sqlite3_bind_double(stmt, 2, utc->outputEndTime);
	return e == SQLITE_OK;
}

static int BindStep(const struct sedml_uniformtimecourse *utc,
					sqlite3_stmt *stmt)
{
	int e = sqlite3_bind_double(stmt, 3, utc->outputEndTime/utc->numberOfPoints);
	return e == SQLITE_OK;
}

static int BindGranularity(const struct sedml_uniformtimecourse *utc,
						   sqlite3_stmt *stmt)
{
	int e;
	if (utc->num_xml_attributes > 0 && utc->xml_attributes) {
		struct sedml_xml_attribute *attr;
		for (int i=0;i<utc->num_xml_attributes;i++) {
			attr = utc->xml_attributes[i];
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

static void Usage(void)
{
	fprintf(stderr, "usage: flint-sedml DB\n");
}

int main(int argc, char *argv[])
{
	int r = EXIT_FAILURE;

	if (argc != 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
		Usage();
		return EXIT_SUCCESS;
	}

	char sedml_file[1024]; // FIXME
	if (!LoadExec(argv[1], sedml_file, NULL)) return EXIT_FAILURE;
	boost::filesystem::path sedml_path = GetPathFromUtf8(sedml_file);
	std::string sedml_path_s = sedml_path.string();

	struct sedml_document *doc = sedml_create_document();
	const struct sedml_sedml *sedml;
	if (!doc) {
		fprintf(stderr, "failed to allocate SED-ML document\n");
		return EXIT_FAILURE;
	}
	if (sedml_read_file(sedml_path_s.c_str(), NULL, doc) < 0) {
		fprintf(stderr, "failed to read SED-ML file: %s\n", sedml_file);
		goto bail0;
	}
	sedml = doc->sedml;
	if (!sedml) {
		fprintf(stderr, "no <sedML> element in SED-ML\n");
		goto bail0;
	}
	if (sedml->num_models <=0 || !sedml->models) {
		fprintf(stderr, "no <model>s in SED-ML\n");
		goto bail0;
	}
	if (sedml->num_simulations <= 0 || !sedml->simulations) {
		fprintf(stderr, "no simulations in SED-ML\n");
		goto bail0;
	}
	if (sedml->num_tasks <= 0 || !sedml->tasks) {
		fprintf(stderr, "no tasks in SED-ML\n");
		goto bail0;
	}

	sqlite3 *db;
	if (sqlite3_open(argv[1], &db) != SQLITE_OK) {
		fprintf(stderr, "failed to open database: %s\n", argv[1]);
		goto bail0;
	}
	char *em;
	int e;
	e = sqlite3_exec(db, "BEGIN", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to start transaction: %s\n", em);
		return EXIT_FAILURE;
	}
	if (!CreateTable(db, "tasks", "(model_id INTEGER, sim_id INTEGER)"))
		goto bail1;
	if (!CreateTable(db, "models", "(model_path TEXT, db_path TEXT)"))
		goto bail1;
	if (!CreateTable(db, "sims", "(algorithm TEXT, length REAL, step REAL, granularity INTEGER)"))
		goto bail1;
	if (!CreateTable(db, "dgs", "(task_id INTEGER, variable TEXT)"))
		goto bail1;

	for (int i=0;i<sedml->num_tasks;i++) {
		const struct sedml_task *task = sedml->tasks[i];
		if (!task || !task->modelReference || !task->simulationReference) {
			fprintf(stderr, "invalid task in SED-ML\n");
			goto bail1;
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
			goto bail1;
		}

		sqlite3_stmt *stmt;
		e = sqlite3_prepare_v2(db, "INSERT INTO models VALUES (?, NULL)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to prepare statement: %d\n", e);
			goto bail1;
		}
		e = sqlite3_bind_text(stmt, 1, model->source, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to bind parameter: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to step statement: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
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
			goto bail1;
		}
		if (simulation->simulation_type != SEDML_UNIFORM_TIME_COURSE) {
			fprintf(stderr,
					"simulation other than uniform time course in SED-ML\n");
			goto bail1;
		}

		const struct sedml_uniformtimecourse *utc;
		utc = (const struct sedml_uniformtimecourse *)simulation;

		e = sqlite3_prepare_v2(db, "INSERT INTO sims VALUES (?, ?, ?, ?)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to prepare statement: %d\n", e);
			goto bail1;
		}
		if (!BindAlgorithm(utc, stmt)) {
			fprintf(stderr, "failed to bind algorithm: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
		}
		if (!BindLength(utc, stmt)) {
			fprintf(stderr, "failed to bind length: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
		}
		if (!BindStep(utc, stmt)) {
			fprintf(stderr, "failed to bind step: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
		}
		if (!BindGranularity(utc, stmt)) {
			fprintf(stderr, "failed to bind granularity: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to step statement: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
		}
		sqlite3_finalize(stmt);

		sqlite3_int64 sim_id = sqlite3_last_insert_rowid(db);

		e = sqlite3_prepare_v2(db, "INSERT INTO tasks VALUES (?, ?)",
							   -1, &stmt, NULL);
		e = sqlite3_bind_int64(stmt, 1, model_id);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to bind model_id: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
		}
		e = sqlite3_bind_int64(stmt, 2, sim_id);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to bind sim_id: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			fprintf(stderr, "failed to step statement: %d\n", e);
			sqlite3_finalize(stmt);
			goto bail1;
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
				e = sqlite3_bind_int64(stmt, 1, task_id);
				if (e != SQLITE_OK) {
					fprintf(stderr, "failed to bind task_id: %d\n", e);
					sqlite3_finalize(stmt);
					goto bail1;
				}
				e = sqlite3_bind_text(stmt, 2, variable->target, -1, SQLITE_STATIC);
				if (e != SQLITE_OK) {
					fprintf(stderr, "failed to bind variable: %d\n", e);
					sqlite3_finalize(stmt);
					goto bail1;
				}
				e = sqlite3_step(stmt);
				if (e != SQLITE_DONE) {
					fprintf(stderr, "failed to step statement: %d\n", e);
					sqlite3_finalize(stmt);
					goto bail1;
				}
				sqlite3_finalize(stmt);
			}
		}
	}

	e = sqlite3_exec(db, "COMMIT", NULL, NULL, &em);
	if (e != SQLITE_OK) {
		fprintf(stderr, "failed to commit transaction: %s\n", em);
		return EXIT_FAILURE;
	}
	r = EXIT_SUCCESS;

 bail1:
	sqlite3_close(db);
 bail0:
	sedml_destroy_document(doc);
	return r;
}
