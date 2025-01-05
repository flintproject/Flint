DATADIR = @top_srcdir@/test/isdmerge

TESTS = no-units.sh units.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk) : test.mk.in
	m4 -DDATADIR=$(DATADIR) -DTEST=$(@:.mk=) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.dat *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
