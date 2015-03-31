DATADIR = @top_srcdir@/test/flint-sbml
MODELDIR = @top_srcdir@/test/models

TESTS = \
    BIOMD0000000114.sh \
    BIOMD0000000152.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DEXPECTED=$(DATADIR)/$(@:.mk=.o.txt) -DINPUT=$(MODELDIR)/$(@:.mk=.xml) -DOUTPUT=$(@:.mk=) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=) $(TESTS:.sh=.mk)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
