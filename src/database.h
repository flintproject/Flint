/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DATABASE_H_
#define FLINT_DATABASE_H_

#include "sqlite3.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * `given_file' and `model_file' have to be >= 1024 byte-length.
 * Note that db is for read only.
 */
int FindGivenFile(sqlite3 *db, char *given_file);
int FindModelFile(sqlite3 *db, char *model_file);

/*
 * `given_file' and `model_file' should be a UTF-8 filename with < 1024 byte-length.
 */
int SaveGivenFile(sqlite3 *db, const char *given_file);
int SaveModelFile(sqlite3 *db, const char *model_file);

/*
 * Both `sedml_file' and `phsp_file' have to be >= 1024 byte-length.
 * Note that db is for read only.
 */
int LoadExec(sqlite3 *db, char *sedml_file, char *phsp_file);

/*
 * Both `sedml_file' and `phsp_file' should be UTF-8 filenames
 * with < 1024 byte-length.
 */
int SaveExec(sqlite3 *db, const char *sedml_file, const char *phsp_file);

#ifdef __cplusplus
}
#endif

#endif
