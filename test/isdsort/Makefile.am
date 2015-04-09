DATADIR = @top_srcdir@/test/isdsort

TESTS = \
	no-units.sh \
	units.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DDATADIR=$(DATADIR) -DTEST=$(@:.mk=) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.isd 0.out/* 2.out/* 9.out/*

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
