# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
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

flint.msi: flint.wixobj flint.exe
	light -ext WixUIExtension $<

timestamp: flint.msi
	install -p $< flint-`date +%Y%m%d`.msi
