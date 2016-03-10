/* -*- Mode: C+; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_HELPER_H_
#define FLINT_DB_HELPER_H_

#define VARIABLES_SCHEMA "(space_id BLOB, type TEXT, id INTEGER, name TEXT, unit TEXT, ncols INTEGER, nrows INTEGER, capacity REAL)"

#define REACHES_SCHEMA "(output_uuid BLOB, output_id INTEGER, input_uuid BLOB, input_id INTEGER, reduction INTEGER)"

#define TACS_SCHEMA "(uuid BLOB, name TEXT, noir INTEGER, nod INTEGER, body TEXT)"

#endif
