#!/bin/bash

rm -rf data
rm -rf src/components/castle-engine
rm -rf src/components/neural-api
rm -rf src/components/synalist
rm -rf src/components/TPocketSphinx
rm -rf src/components/internettools
rm -rf src/components/richmemo
rm -rf src/components/KControls
rm -rf src/components/HtmlViewer
rm -rf src/components/cge-spine-mixer
rm -rf src/components/SynFacilSyn         
rm -rf src/components/brookfreepascal
rm -rf src/components/spine-runtimes
rm -rf src/components/JsonTools
rm -rf src/components/pascal-webui

git clone --depth 1 https://github.com/Kagamma/satania-buddy-data.git data
cd src/components
git clone https://github.com/Kagamma/castle-engine.git
cd castle-engine
git checkout satania-buddy
cd ..
svn checkout https://svn.code.sf.net/p/synalist/code/trunk synalist
git clone --depth 1 https://github.com/Kagamma/internettools.git
git clone --depth 1 https://github.com/kryslt/KControls.git
git clone --depth 1 https://github.com/Kagamma/JsonTools.git
git clone --depth 1 https://github.com/t-edson/SynFacilSyn.git
git clone --depth 1 https://github.com/Kagamma/pascal-webui.git
git clone https://github.com/Kagamma/HtmlViewer.git
cd HtmlViewer
git checkout satania-buddy
cd ..
git clone --depth 1 https://github.com/Kagamma/cge-spine-mixer.git

cd ..
