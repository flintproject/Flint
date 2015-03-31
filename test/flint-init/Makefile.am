EXECUTABLE = @top_builddir@/src/flint-init

TESTS = \
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
    divide.sh \
    eq.sh \
    exp.sh \
    floor.sh \
    geq.sh \
    gt.sh \
    leq.sh \
    ln.sh \
    lt.sh \
    max.sh \
    min.sh \
    neq.sh \
    plus.sh \
    power.sh \
    rem.sh \
    sec.sh \
    sech.sh \
    sin.sh \
    sinh.sh \
    tan.sh \
    tanh.sh \
    times.sh

$(TESTS):
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f ../common/runtime/$(@:.sh=.mk) EXECUTABLE=$(EXECUTABLE) clean all check' >> $@
	chmod +x $@

MOSTLYCLEANFILES = $(TESTS) test-* bc-*
