EXECUTABLE = @top_builddir@/src/flint-bcc
DATADIR = @top_srcdir@/test/flint-bcc

CALL1_TESTS = \
    abs.sh \
    arccos.sh \
    arccosh.sh \
    arccot.sh \
    arccoth.sh \
    arccsc.sh \
    arccsch.sh \
    arcsec.sh \
    arcsech.sh \
    arcsin.sh \
    arcsinh.sh \
    arctan.sh \
    arctanh.sh \
    ceiling.sh \
    cos.sh \
    cosh.sh \
    cot.sh \
    coth.sh \
    csc.sh \
    csch.sh \
    exp.sh \
    floor.sh \
    ln.sh \
    log10.sh \
    sec.sh \
    sech.sh \
    sin.sh \
    sinh.sh \
    tan.sh \
    tanh.sh

CALL2_TESTS = \
    divide.sh \
    eq.sh \
    geq.sh \
    gt.sh \
    leq.sh \
    log.sh \
    lt.sh \
    max.sh \
    min.sh \
    neq.sh \
    plus.sh \
    power.sh \
    times.sh

EXTRA_CALL1_TESTS = \
    exponential_variate.sh \
    poisson_variate.sh

EXTRA_CALL2_TESTS = \
    gamma_variate.sh \
    gauss_variate.sh \
    uniform_variate.sh

CALL_TESTS = $(CALL1_TESTS) $(CALL2_TESTS) $(EXTRA_CALL1_TESTS) $(EXTRA_CALL2_TESTS)

REGRESSION_TESTS = \
	duplicate-labels.sh

TESTS = $(CALL_TESTS) $(REGRESSION_TESTS)

$(TESTS):
	echo '#!/bin/sh' > $@
	echo '$(MAKE) -f $(@:.sh=.mk) -s clean all check' >> $@
	chmod +x $@

$(CALL1_TESTS:.sh=.i.txt): $(DATADIR)/test-call1.in
	m4 -DCALL1=$(@:.i.txt=) $< > $@

$(CALL2_TESTS:.sh=.i.txt): $(DATADIR)/test-call2.in
	m4 -DCALL2=$(@:.i.txt=) $< > $@

$(EXTRA_CALL1_TESTS:.sh=.i.txt): $(DATADIR)/test-call1.in
	m4 -DCALL1=\$$$(@:.i.txt=) $< > $@

$(EXTRA_CALL2_TESTS:.sh=.i.txt): $(DATADIR)/test-call2.in
	m4 -DCALL2=\$$$(@:.i.txt=) $< > $@

$(CALL_TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DEXECUTABLE=$(EXECUTABLE) -DINPUT=$(@:.mk=.i.txt) -DOUTPUT=$(@:.mk=) $< > $@

$(REGRESSION_TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DEXECUTABLE=$(EXECUTABLE) -DINPUT=$(DATADIR)/$(@:.mk=.i.txt) -DOUTPUT=$(@:.mk=) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=) $(TESTS:.sh=.i.txt) $(TESTS:.sh=.mk)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.i.txt) $(TESTS:.sh=.mk)
