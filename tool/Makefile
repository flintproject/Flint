# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
EXAMPLES := $(wildcard example/*.ss)

.PHONY: all check phml clean

all: index.html $(foreach ss,$(EXAMPLES),$(addsuffix .phml,$(basename $(ss))))

check:
	$(MAKE) -C test check

clean:
	-rm index.html example/*.phml

index.html: index.ss
	scheme --script $< example > $@

%.phml: %.ss ss2phml.ss
	scheme --script ss2phml.ss $< $$(uuid) > $@
