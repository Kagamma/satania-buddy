rm -rf src/components/castle-engine
rm -rf src/components/neural-api   
rm -rf src/components/csscontrols
rm -rf src/components/synalist

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
cd ..

