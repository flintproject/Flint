DATADIR = @top_srcdir@/test/flint-nc
MODELDIR = @top_srcdir@/test/models

TESTS = \
	Izhikevich_2003_model.isml.sh \
	LR1-ring_reentry_initiate.isml.sh \
	Rybak_2006_with_static_instance_and_multiple_input.isml.sh \
	bogus-td.phml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DEXPECTED=$(DATADIR)/$(@:.mk=.txt) -DINPUT=$(MODELDIR)/$(@:.mk=) -DOUTPUT=$(@:.mk=.output) $< > $@

CLEANFILES = *.output *.output.db *.output.pb *.sh *.mk

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
