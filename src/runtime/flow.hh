/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_FLOW_HH_
#define FLINT_RUNTIME_FLOW_HH_

#include <set>
#include <boost/ptr_container/ptr_map.hpp>

#include "sqlite3.h"

typedef boost::ptr_map<int, std::set<int> > FlowInboundMap;
typedef boost::ptr_map<int, std::set<int> > FlowOutboundMap;

/*
 * Note that db is for read only.
 */
bool LoadFlows(sqlite3 *db, FlowInboundMap *im, FlowOutboundMap *om);

#endif
