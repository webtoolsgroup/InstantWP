#!/bin/sh

echo "Mounting IWPServer WebDav Volume..."
cd "$(dirname "$0")"
osascript ./mountWebDav.scpt
