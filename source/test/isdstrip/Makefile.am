DATADIR = @top_srcdir@/test/isdstrip

TESTS = \
	no-units.sh \
	random.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s all check' >> $@
	chmod +x $@

no-units.mk:  $(DATADIR)/no-units.mk.in
	m4 -DDATADIR=$(DATADIR) $< > $@

random.mk: $(DATADIR)/random.mk.in
	m4 $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.csv *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
