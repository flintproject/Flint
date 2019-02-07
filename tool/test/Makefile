# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
.PHONY: check clean

check: test.ss
	scheme --libdirs "$(CURDIR)/xunit:$(CURDIR)/../" --script $<

clean:
