DATADIR = @top_srcdir@/test/flint-sbmlenc
MODELDIR = @top_srcdir@/test/models

TESTS = \
    BIOMD0000000114.sh \
    BIOMD0000000152.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DEXPECTED=$(DATADIR)/$(@:.mk=) -DINPUT=$(MODELDIR)/$(@:.mk=.xml) -DOUTPUT=$(@:.mk=.out) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=) $(TESTS:.sh=.mk) $(TESTS:.sh=.out/*)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
