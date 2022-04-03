init:
	sh ./install-submodules.sh
	sh ./install-packages.sh

build:
	lazbuild src/SataniaDesktopAssistant.lpr

build-linux64:
	lazbuild --operating-system=Linux src/SataniaDesktopAssistant.lpr

clean:
	rm -rf lib
	rm *.dbg
	rm satania-buddy.exe
	rm satania-buddy
