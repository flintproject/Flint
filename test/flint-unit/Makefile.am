DATADIR = @top_srcdir@/test/flint-unit
MODELDIR = @top_srcdir@/test/models

TESTS = \
	self-circular-unit.phml.sh \
	time_units.isml.sh

XFAIL_TESTS = self-circular-unit.phml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DINPUT=$(MODELDIR)/$(@:.mk=) -DOUTPUT=$(@:.mk=.output) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) $(TESTS:.sh=.output) $(TESTS:.sh=.output.db) $(TESTS:.sh=.output.db-journal)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
