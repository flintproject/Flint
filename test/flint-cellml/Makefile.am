DATADIR = @top_srcdir@/test/flint-cellml
MODELDIR = @top_srcdir@/test/models

TESTS = \
    lockwood_ewy_hermann_holford_2006.sh \
    yang_tong_mccarver_hines_beard_2006.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DINPUT=$(MODELDIR)/$(@:.mk=.cellml) \
		-DOUTPUT=$(@:.mk=) \
		$< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=) $(TESTS:.sh=.mk) $(TESTS:.sh=.stderr)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
