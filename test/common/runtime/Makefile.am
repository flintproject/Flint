include $(top_srcdir)/common.mk

BCC_EXECUTABLE = @abs_top_builddir@/src/flint-bcc
LAYOUT_EXECUTABLE = @abs_top_builddir@/src/flint-layout

.dat1.nl1:
	nl $< > $@

.dat2.nl2:
	nl $< > $@

.nl1.mk:
	echo "BC = \\" > $@
	cut -f1 $< | sed -e 's,^ *\([0-9]*\),bc-$*-\1 \\,' >> $@
	echo >> $@
	echo "TEST = \\" >> $@
	cut -f1 $< | sed -e 's,^ *\([0-9]*\),test-$*-\1 \\,' >> $@
	echo >> $@
	echo "CHECK = \\" >> $@
	cut -f1 $< | sed -e 's,^ *\([0-9]*\),check-$*-\1 \\,' >> $@
	echo >> $@
	echo '.PHONY: all check $$(CHECK) clean' >> $@
	echo >> $@
	echo 'all: $$(BC) $$(TEST)' >> $@
	echo >> $@
	echo 'check: $$(CHECK)' >> $@
	echo >> $@
	echo 'clean:' >> $@
	echo '	-rm -f $$(TEST) $$(BC)' >> $@
	echo >> $@
	echo 'test-db:' >> $@
	echo '	flint-singleton $$@' >> $@
	echo >> $@
	echo 'test-layout: test-db @abs_builddir@/test-name' >> $@
	echo '	$(LAYOUT_EXECUTABLE) $$^ $$@' >> $@
	echo >> $@
	sed -e 's,^ *\(.*\)\t\([^ ]*\).*$$,bc-$*-\1: @abs_srcdir@/call1.in\n\tm4 -DCALL1=$* -DARG0=\2 $$< | $(BCC_EXECUTABLE) > $$@,' $< >> $@
	echo >> $@
	sed -e 's,^ *\(.*\)\t.*$$,test-$*-\1: test-layout bc-$*-\1\n\t$$(EXECUTABLE) $$^ /dev/null $$@,' $< >> $@
	echo >> $@
	sed -e 's,^ *\(.*\)\t[^ ]* \([^ ]*\),check-$*-\1: test-$*-\1\n\t@abs_builddir@/x_check \2 $$<,' $< >> $@

.nl2.mk:
	echo "BC = \\" > $@
	cut -f1 $< | sed -e 's,^ *\([0-9]*\),bc-$*-\1 \\,' >> $@
	echo >> $@
	echo "TEST = \\" >> $@
	cut -f1 $< | sed -e 's,^ *\([0-9]*\),test-$*-\1 \\,' >> $@
	echo >> $@
	echo "CHECK = \\" >> $@
	cut -f1 $< | sed -e 's,^ *\([0-9]*\),check-$*-\1 \\,' >> $@
	echo >> $@
	echo '.PHONY: all check $$(CHECK) clean' >> $@
	echo >> $@
	echo 'all: $$(BC) $$(TEST)' >> $@
	echo >> $@
	echo 'check: $$(CHECK)' >> $@
	echo >> $@
	echo 'clean:' >> $@
	echo '	-rm -f $$(TEST) $$(BC)' >> $@
	echo >> $@
	echo 'test-db:' >> $@
	echo '	flint-singleton $$@' >> $@
	echo >> $@
	echo 'test-layout: test-db @abs_builddir@/test-name' >> $@
	echo '	$(LAYOUT_EXECUTABLE) $$^ $$@' >> $@
	echo >> $@
	sed -e 's,^ *\(.*\)\t\([^ ]*\) \([^ ]*\).*$$,bc-$*-\1: @abs_srcdir@/call2.in\n\tm4 -DCALL2=$* -DARG0=\2 -DARG1=\3 $$< | $(BCC_EXECUTABLE) > $$@,' $< >> $@
	echo >> $@
	sed -e 's,^ *\(.*\)\t.*$$,test-$*-\1: test-layout bc-$*-\1\n\t$$(EXECUTABLE) $$^ /dev/null $$@,' $< >> $@
	echo >> $@
	sed -e 's,^ *\(.*\)\t[^ ]* [^ ]* \([^ ]*\),check-$*-\1: test-$*-\1\n\t@abs_builddir@/x_check \2 $$<,' $< >> $@

test-name:
	echo '00000000-0000-0000-0000-000000000000 v 1 a' > $@
	echo '00000000-0000-0000-0000-000000000000 v 2 b' >> $@
	echo '00000000-0000-0000-0000-000000000000 v 3 x' >> $@

MK_FILES = \
    abs.mk \
    arccos.mk \
    arccosh.mk \
    arccot.mk \
    arccoth.mk \
    arccsc.mk \
    arccsch.mk \
    arcsec.mk \
    arcsech.mk \
    arcsin.mk \
    arcsinh.mk \
    arctan.mk \
    arctanh.mk \
    ceiling.mk \
    cos.mk \
    cosh.mk \
    cot.mk \
    coth.mk \
    csc.mk \
    csch.mk \
    divide.mk \
    eq.mk \
    exp.mk \
    floor.mk \
    geq.mk \
    gt.mk \
    leq.mk \
    ln.mk \
    log.mk \
    log10.mk \
    lt.mk \
    max.mk \
    min.mk \
    neq.mk \
    plus.mk \
    power.mk \
    rem.mk \
    sec.mk \
    sech.mk \
    sin.mk \
    sinh.mk \
    tan.mk \
    tanh.mk \
    times.mk

MOSTLYCLEANFILES = test-name $(MK_FILES) x_check

noinst_DATA = test-name $(MK_FILES)

noinst_PROGRAMS = x_check

x_check_SOURCES = x_check.c
