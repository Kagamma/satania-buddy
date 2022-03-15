init:
	sh ./install-submodules.sh
	sh ./install-packages.sh

build:
	lazbuild src/SataniaDesktopAssistant.lpr

clean:
	rm -rf lib
	rm *.dbg
	rm x86_64-*
