DATADIR = @abs_top_srcdir@/test/flint-phml-xfail
MODELDIR = @abs_top_srcdir@/test/models

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
	echo 'install -d $(@:.sh=)' >> $@
	echo '$(MAKE) -C $(@:.sh=) -f ../$< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DINPUT=$(MODELDIR)/$(@:.mk=) \
		-DEXPECTED=$(DATADIR)/$(@:.mk=.txt) \
		$< > $@

CLEANFILES = *.sh *.mk

clean-local:
	-rm -rf $(TESTS:.sh=)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
