DATADIR = @top_srcdir@/test/flint-sort

TESTS = \
    empty.sh \
    euler.sh \
    rk4.sh \
    sbml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DEXPECTED=$(DATADIR)/$(@:.mk=.o.txt) -DDIFF=$(@:.mk=.diff) -DINPUT=$(DATADIR)/$(@:.mk=.i.txt) -DOUTPUT=$(@:.mk=) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=) $(TESTS:.sh=.diff) $(TESTS:.sh=.mk)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
