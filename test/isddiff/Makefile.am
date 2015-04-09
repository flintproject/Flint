DATADIR = @top_srcdir@/test/isddiff

CSV_TESTS = \
	no-units.sh \
	units.sh

TESTS = $(CSV_TESTS) seed.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(CSV_TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DDATADIR=$(DATADIR) -DTEST=$(@:.mk=) $< > $@

seed.mk: $(DATADIR)/seed.mk.in
	m4 $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
