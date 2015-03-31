/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DATABASE_H_
#define FLINT_DATABASE_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
 * `model_file' have to be >= 1024 byte-length.
 */
int FindModelFile(const char *db_file, char *model_file);

/*
 * `model_file' should be a UTF-8 filename with < 1024 byte-length.
 */
int SaveModelFile(const char *db_file, const char *model_file);

/*
 * Both `sedml_file' and `phsp_file' have to be >= 1024 byte-length.
 */
int LoadExec(const char *db_file, char *sedml_file, char *phsp_file);

/*
 * Both `sedml_file' and `phsp_file' should be UTF-8 filenames
 * with < 1024 byte-length.
 */
int SaveExec(const char *db_file, const char *sedml_file, const char *phsp_file);

#ifdef __cplusplus
}
#endif

#endif
