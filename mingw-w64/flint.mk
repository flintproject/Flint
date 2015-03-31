# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
MSI_DEPENDENCY = flint.jar $(foreach j,$(JARS),$(j).jar) $(foreach d,$(BIN_DLLS) $(BOOST_DLLS) $(EXT_DLLS),lib/$(d).dll) $(foreach e,$(EXES) $(EXT_EXES),$(e).exe)

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
