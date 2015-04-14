DATADIR = @top_srcdir@/test/flint-phz
MODELDIR = @top_srcdir@/test/models

TESTS = \
	damping_oscillation.sh \
	timeseries.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DINPUT_PHZ=$(MODELDIR)/$(@:.mk=.phz) \
		-DINPUT_DB=$(@:.mk=.input.db) \
		-DOUTPUT_DB=$(@:.mk=.output.db) \
		-DTARGETDIR=$(@:.mk=) \
		$< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.db

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
