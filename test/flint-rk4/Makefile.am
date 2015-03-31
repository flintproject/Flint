EXECUTABLE = @top_builddir@/src/flint-rk4
DATADIR = @top_srcdir@/test/flint-rk4
STATEMENTDIR = @top_srcdir@/test/data/statement

TESTS = \
    conditional.sh \
    empty.sh \
    function.sh \
    literal.sh \
    ode.sh \
    sbml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DEXECUTABLE=$(EXECUTABLE) -DEXPECTED=$(DATADIR)/$(@:.mk=.o.txt) -DDIFF=$(@:.mk=.diff) -DINPUT=$(STATEMENTDIR)/$(@:.mk=.i.txt) -DOUTPUT=$(@:.mk=) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=) $(TESTS:.sh=.diff) $(TESTS:.sh=.mk)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
