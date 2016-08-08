/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/bc.h"

namespace flint {

Bytecode::Bytecode()
	: nol(0)
	, shv(new ShVector)
	, bhv(new BhVector)
	, cv(new CVector)
{
}

}
