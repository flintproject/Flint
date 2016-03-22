/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_BACKGROUND_H_
#define FLINT_BACKGROUND_H_

namespace flint {

/*
 * Given a filename for lock, initialize the current process as a background
 * one.
 */
void InitializeBackgroundProcess(const char *filename);

}

#endif
