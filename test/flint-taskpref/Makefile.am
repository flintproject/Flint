DATADIR = @top_srcdir@/test/flint-taskpref
MODELDIR = @top_srcdir@/test/models

TESTS = \
    Izhikevich_2003.xml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DEXPECTED_STDOUT=$(DATADIR)/$(@:.mk=.stdout) \
		-DINPUT=$(MODELDIR)/$(@:.mk=) \
		-DOUTPUT_DB=$(@:.mk=.db) \
		-DOUTPUT_STDOUT=$(@:.mk=.stdout) \
		$< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.db *.stdout

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
