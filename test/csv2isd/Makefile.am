DATADIR = @top_srcdir@/test/csv2isd

TESTS = \
	no-steps.sh \
	no-units.sh \
	x.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): @top_srcdir@/test/csv2isd/test.mk.in
	m4 -DINPUT=$(DATADIR)/$(@:.mk=.csv) -DOUTPUT=$(@:.mk=.isd) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.diff *.csv *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
