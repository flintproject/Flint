DATADIR = @abs_top_srcdir@/test/flint-unitoftime
MODELDIR = @abs_top_srcdir@/test/models

TESTS = \
	self-circular-unit.phml.sh \
	time_units.isml.sh

XFAIL_TESTS = self-circular-unit.phml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo 'install -d $(@:.sh=)' >> $@
	echo '$(MAKE) -C $(@:.sh=) -f ../$< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DINPUT=$(MODELDIR)/$(@:.mk=) \
		-DEXPECTED=$(DATADIR)/$(@:.mk=.bin) \
		$< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk)

clean-local:
	-rm -rf $(TESTS:.sh=)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
