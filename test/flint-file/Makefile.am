DATADIR = @top_srcdir@/test/flint-file
MODELDIR = @top_srcdir@/test/models

TESTS = \
	BIOMD0000000114.xml.sh \
	Izhikevich_2003.phsp.sh \
	Izhikevich_2003.xml.sh \
	LR-pulse-189-164.isml.sh \
	damping_oscillation.phz.sh \
	empty.phml.sh \
	lockwood_ewy_hermann_holford_2006.cellml.sh \
	mathml_cosh.xml.sh \
	yang_tong_mccarver_hines_beard_2006.cellml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DEXPECTED=$(DATADIR)/$(@:.mk=.txt) -DINPUT=$(MODELDIR)/$(@:.mk=) -DOUTPUT=$(@:.mk=.output) $< > $@

CLEANFILES = *.db *.output *.sh *.mk

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
