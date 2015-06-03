TESTDIR = @abs_top_srcdir@/test
DATADIR = $(TESTDIR)/flint-var

CELLML_TESTS = \
    lockwood_ewy_hermann_holford_2006.cellml.sh \
    yang_tong_mccarver_hines_beard_2006.cellml.sh

PHML_TESTS = \
    Rybak_2006_with_static_instance_and_multiple_input.isml.sh \
    hepatocyte_external.isml.sh \
    hepatocyte_internal.isml.sh \
    ringed_Beeler_Reuter_1977_model_with_static_instance.isml.sh

SBML_TESTS = \
    BIOMD0000000114.xml.sh \
    BIOMD0000000152.xml.sh

TESTS = $(CELLML_TESTS) $(PHML_TESTS) $(SBML_TESTS)

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo 'install -d $(@:.sh=)' >> $@
	echo '$(MAKE) -C $(@:.sh=) -f ../$< -s clean all check' >> $@
	chmod +x $@

$(CELLML_TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DCOMMAND=flint-cellml \
		-DEXPECTED=$(DATADIR)/$(@:.mk=.bin) \
		-DINPUT=$(TESTDIR)/models/$(@:.mk=) \
		$< > $@

$(PHML_TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DCOMMAND=flint-phml \
		-DEXPECTED=$(DATADIR)/$(@:.mk=.bin) \
		-DINPUT=$(TESTDIR)/models/$(@:.mk=) \
		$< > $@

$(SBML_TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DCOMMAND=flint-sbml \
		-DEXPECTED=$(DATADIR)/$(@:.mk=.bin) \
		-DINPUT=$(TESTDIR)/models/$(@:.mk=) \
		$< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk)

clean-local:
	-rm -rf $(TESTS:.sh=)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
