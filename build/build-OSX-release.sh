#!/bin/bash

echo ----------------------------
echo OSX IWP Release Build Script
echo ----------------------------
cd "${0%/*}"


# some constants
# REL_ROOT=/Users/seamus/Dumpster
REL_ROOT=/Users/seamus/GitHub/InstantWordPress/zzRelease
SOURCE_DIR=/Users/seamus/GitHub/InstantWordPress
VM_FILE=iwpserver-1.1.qcow2

# set release root
REL_DIR=$REL_ROOT/$1

echo Making release directory $REL_DIR
mkdir $REL_DIR/


echo Making build directories...
mkdir $REL_DIR/bin
mkdir $REL_DIR/config
mkdir $REL_DIR/docs
mkdir $REL_DIR/platform
mkdir $REL_DIR/vm

echo Copying files...

# startup files
cp $SOURCE_DIR/MainDev/iwpcli $REL_DIR/
cp $SOURCE_DIR/MainDev/Start-InstantWP $REL_DIR/
cp $SOURCE_DIR/MainDev/Quit-InstantWP $REL_DIR/
cp $SOURCE_DIR/MainDev/ReadMe/ReadMe-OSX.txt $REL_DIR/

# bin directory
cp -R $SOURCE_DIR/MainDev/bin/InstantWP.app $REL_DIR/bin/
cp $SOURCE_DIR/MainDev/bin/iwp $REL_DIR/bin/
cp $SOURCE_DIR/MainDev/bin/ssh-term $REL_DIR/bin/

# config directory
cp $SOURCE_DIR/MainDev/config/iwp-osx.ini $REL_DIR/config/

# doc directory
cp $SOURCE_DIR/MainDev/docs/about.html $REL_DIR/docs
cp $SOURCE_DIR/MainDev/docs/documentation.html $REL_DIR/docs/

# platform directory
cp -R $SOURCE_DIR/MainDev/platform/osx $REL_DIR/platform/

# vm directory
cp $SOURCE_DIR/MainDev/vm/$VM_FILE $REL_DIR/vm

# zipping the release
# echo Making release zip $REL_ROOT/$1.zip
# cd $REL_DIR
# zip -r $REL_ROOT/$1.zip ./$1 -x "*.DS_Store"

echo Done!


