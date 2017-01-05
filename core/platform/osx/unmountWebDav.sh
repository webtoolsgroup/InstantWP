#!/bin/sh
echo "Unmounting IWPServer WebDav Volume..."
cd "$(dirname "$0")"
osascript ./ejectDisk.scpt
