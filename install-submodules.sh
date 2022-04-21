rm -rf data
rm -rf src/components/castle-engine
rm -rf src/components/neural-api
rm -rf src/components/typing-label
rm -rf src/components/synalist
rm -rf src/components/TPocketSphinx
rm -rf src/components/internettools
rm -rf src/components/richmemo
rm -rf src/components/KControls
rm -rf src/components/HtmlViewer

git clone --depth 1 https://github.com/Kagamma/satania-buddy-data.git data
cd src/components
git clone --depth 1 https://github.com/castle-engine/castle-engine.git
cd castle-engine
git apply ../patches/castle-engine.patch
cd ..
git clone --depth 1 https://github.com/joaopauloschuler/neural-api
cd neural-api
git apply ../patches/neural-api.patch
cd ..
git clone https://gitlab.com/EugeneLoza/typing-label.git
svn checkout https://svn.code.sf.net/p/synalist/code/trunk synalist
git clone --depth 1 https://github.com/Kagamma/internettools.git
git clone --depth 1 https://github.com/kryslt/KControls.git
git clone --depth 1 https://github.com/Kagamma/HtmlViewer.git
git clone https://github.com/pst2d/csscontrols.git
cd csscontrols
git checkout dev
cd ..
cd ..
