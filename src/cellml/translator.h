/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CELLML_TRANSLATOR_H_
#define FLINT_CELLML_TRANSLATOR_H_

bool TranslateCellml(const char *db_file,
					 const char *iv_file,
					 const char *function_file,
					 const char *ode_file);

#endif
