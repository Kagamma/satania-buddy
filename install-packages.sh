lazbuild --add-package-link src/components/castle-engine/src/vampyre_imaginglib/src/Packages/VampyreImagingPackageExt.lpk
lazbuild --add-package-link src/components/richmemo/richmemopackage.lpk
lazbuild --build-ide= --add-package src/components/castle-engine/packages/castle_base.lpk src/components/castle-engine/packages/castle_components.lpk src/components/richmemo/ide/richmemo_design.lpk src/components/HtmlViewer/package/FrameViewer09.lpk
lazbuild --add-package-link src/components/castle-engine/packages/castle_window.lpk
