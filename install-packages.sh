lazbuild --add-package-link src/components/castle-engine/src/vampyre_imaginglib/src/Packages/VampyreImagingPackageExt.lpk
lazbuild --build-ide= --add-package src/components/castle-engine/packages/castle_base.lpk src/components/castle-engine/packages/castle_components.lpk
lazbuild --add-package-link src/components/castle-engine/packages/castle_window.lpk
