DATADIR = @top_srcdir@/test/flint-phml-xfail
MODELDIR = @top_srcdir@/test/models

TESTS = \
	double_pendulum.isml.sh \
	self-circular-unit.phml.sh \
	state-to-static.phml.sh \
	swapped-definitions.phml.sh \
	variable-to-static.phml.sh \
	x-capsulated-by.phml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DINPUT=$(MODELDIR)/$(@:.mk=) \
		-DOUTPUT=$(@:.mk=.output) \
		-DSTDERR=$(@:.mk=.stderr) \
		-DEXPECTED=$(DATADIR)/$(@:.mk=.txt) \
		$< > $@

CLEANFILES = *.output *.output-journal *.stderr *.sh *.mk

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
