#!/bin/bash

echo ----------------------------
echo OSX IWP Release Build Script
echo ----------------------------
cd "${0%/*}"


# some constants
# REL_ROOT=/Users/seamus/Dumpster

mkdir /Users/seamus/GitHub/InstantWP/build/release
REL_ROOT=/Users/seamus/GitHub/InstantWP/build/release
SOURCE_DIR=/Users/seamus/GitHub/InstantWP
VM_FILE=iwpserver-2.0.2.qcow2

# set release root
REL_DIR=$REL_ROOT/$1

echo Making release directory $REL_DIR
mkdir $REL_DIR/


echo Making build directories...
mkdir $REL_DIR/bin
mkdir $REL_DIR/config
mkdir $REL_DIR/docs
mkdir $REL_DIR/docs/images
mkdir $REL_DIR/platform
mkdir $REL_DIR/vm
mkdir $REL_DIR/controlpanel


echo Copying files...

# startup files
cp $SOURCE_DIR/core/iwpcli $REL_DIR/
cp $SOURCE_DIR/core/Start-InstantWP $REL_DIR/
cp $SOURCE_DIR/core/ReadMe/ReadMe-First-macOS.html $REL_DIR/

# bin directory
cp $SOURCE_DIR/core/bin/iwp $REL_DIR/bin/
cp $SOURCE_DIR/core/bin/ssh-term $REL_DIR/bin/
cp $SOURCE_DIR/core/bin/run-iwpcli $REL_DIR/bin/
cp $SOURCE_DIR/core/bin/startIWP $REL_DIR/bin/

# config directory
cp $SOURCE_DIR/core/config/iwp-osx.ini $REL_DIR/config/

# doc directory
cp $SOURCE_DIR/core/docs/about.html $REL_DIR/docs
cp $SOURCE_DIR/core/docs/documentation.html $REL_DIR/docs/
cp $SOURCE_DIR/core/docs/InstantWP-User-Guide.pdf $REL_DIR/docs/
cp $SOURCE_DIR/core/docs/LICENSE.txt $REL_DIR/LICENSE.txt
cp -R $SOURCE_DIR/core/docs/images $REL_DIR/docs/

# control panel dir
cp -R $SOURCE_DIR/core/controlpanel/distribute/InstantWP.app $REL_DIR/controlpanel/
cp -R $SOURCE_DIR/core/images $REL_DIR/images
cp $SOURCE_DIR/core/controlpanel/start-ui $REL_DIR/controlpanel/

# platform directory
cp -R $SOURCE_DIR/core/platform/osx $REL_DIR/platform/

# vm directory
cp $SOURCE_DIR/core/vm/$VM_FILE $REL_DIR/vm

# zipping the release
# echo Making release zip $REL_ROOT/$1.zip
# cd $REL_DIR
# zip -r $REL_ROOT/$1.zip ./$1 -x "*.DS_Store"

echo Done!


