EXECUTABLE = @top_builddir@/src/flint-tac
DATADIR = @top_srcdir@/test/flint-tac

TESTS = \
    test-and.sh \
    test-log.sh \
    test-neq.sh \
    test-not-and.sh \
    test-not-eq.sh \
    test-not-geq.sh \
    test-not-gt.sh \
    test-not-leq.sh \
    test-not-lt.sh \
    test-not-neq.sh \
    test-not-not.sh \
    test-not-or.sh \
    test-not-xor.sh \
    test-or.sh \
    test-xor.sh \
    test-Delay.sh \
    test-DeltaTime.sh \
    test-exponential_variate.sh \
    test-gamma_variate.sh \
    test-gauss_variate.sh \
    test-poisson_variate.sh \
    test-uniform_variate.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DEXECUTABLE=$(EXECUTABLE) -DEXPECTED=$(DATADIR)/$(@:.mk=.o.txt) -DDIFF=$(@:.mk=.diff) -DINPUT=$(DATADIR)/$(@:.mk=.i.txt) -DOUTPUT=$(@:.mk=) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=) $(TESTS:.sh=.diff) $(TESTS:.sh=.mk)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
