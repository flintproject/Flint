TESTDIR = @top_srcdir@/test
DATADIR = $(TESTDIR)/flint-layout

TESTS = \
    Rybak_2006_with_static_instance_and_multiple_input.isml.sh \
    ringed_Beeler_Reuter_1977_model_with_static_instance.isml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DINPUT=$(TESTDIR)/models/$(@:.mk=) \
		-DOUTPUT=$(@:.mk=.output) \
		$< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.output) $(TESTS:.sh=.output.db) $(TESTS:.sh=.mk)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
