#!/bin/bash

echo ----------------------------
echo OSX IWP Code Sign Script
echo ----------------------------
cd "${0%/*}"

echo Fixing Racket Framework setup
# Create Resources folder and copy Info.plist
mkdir ./controlpanel/distribute/InstantWP.app/Contents/Frameworks/Racket.framework/Versions/6.9_3m/Resources
cp /Users/seamus/GitHub/InstantWP/core/controlpanel/resource/AppBundle/Frameworks/Racket.framework/Versions/6.9_3m/Resources/Info.plist ./controlpanel/distribute/InstantWP.app/Contents/Frameworks/Racket.framework/Versions/6.9_3m/Resources/
# Create softlinks
cd ./controlpanel/distribute/InstantWP.app/Contents/Frameworks/Racket.framework/Versions/
ln -s ./6.9_3m Current
cd /Users/seamus/GitHub/InstantWP/core/controlpanel/distribute/InstantWP.app/Contents/Frameworks/Racket.framework/
ln -s ./Versions/Current/Racket Racket
ln -s ./Versions/Current/Resources Resources
cd /Users/seamus/GitHub/InstantWP/core/

echo Verify Start-InstantWP.app ...
codesign -vvv -d ./Start-InstantWP.app
spctl --verbose --assess --type  execute ./Start-InstantWP.app

echo Sign InstantWP.app ...

echo Sign Racket framework...
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Frameworks/Racket.framework/Versions/6.9_3m/

echo Sign plugins...
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/MMTabBarView.framework/MMTabBarView
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libcairo.2.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libcrypto.1.0.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libexpat.1.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libffi.6.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libfontconfig.1.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libfreetype.6.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libglib-2.0.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libgmodule-2.0.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libgobject-2.0.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libgthread-2.0.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libharfbuzz.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libintl.8.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libjpeg.9.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libpango-1.0.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libpangocairo-1.0.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libpangoft2-1.0.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libpixman-1.0.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libpng16.16.dylib
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/Resources/exts/ert/r1/libssl.1.0.0.dylib


echo Sign app binary...
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app/Contents/MacOS/InstantWP
codesign --force --verify --verbose --sign "Developer ID Application: Seamus Brady (XR72JCQSHA)" ./controlpanel/distribute/InstantWP.app

echo Verify InstantWP.app ...
codesign -vvv -d ./controlpanel/distribute/InstantWP.app
spctl --verbose --assess --type  execute ./controlpanel/distribute/InstantWP.app

echo Done!



