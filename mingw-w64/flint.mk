# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include header.mk

MSI_DEPENDENCY = \
    flint.jar \
    $(foreach j,$(JARS),lib/$(j).jar) \
    $(foreach d,$(BIN_DLLS) $(BOOST_DLLS) wxmsw310u_gcc_custom,$(d).dll) \
    $(foreach e,$(EXES),$(e).exe)

.PHONY: all clean install uninstall timestamp

all: timestamp

clean:
	-rm -rf flint.exe flint.msi *.wixpdb *.wixobj

install: flint.msi
	msiexec /i $<

uninstall: flint.msi
	msiexec /x $<

flint.exe: flint.xml
	launch4jc $<

flint.wixobj: flint.wxs
	candle $<

flint.msi: flint.wixobj flint.exe $(MSI_DEPENDENCY)
	light -ext WixUIExtension $<

timestamp: flint.msi
	install -p $< flint-`date +%Y%m%d`.msi
