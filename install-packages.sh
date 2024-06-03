#!/bin/bash

lazbuild --add-package-link src/components/castle-engine/src/vampyre_imaginglib/src/Packages/VampyreImagingPackageExt.lpk
lazbuild --build-ide= --add-package src/components/castle-engine/packages/castle_base.lpk src/components/castle-engine/packages/castle_components.lpk src/components/HtmlViewer/package/FrameViewer09.lpk src/components/KControls/packages/lazarus/kcontrolsbase.lpk src/components/KControls/packages/lazarus/kcontrolslaz.lpk
lazbuild --add-package-link src/components/castle-engine/packages/castle_window.lpk
