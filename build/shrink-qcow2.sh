#!/bin/bash

echo ----------------------------
echo    QEMU Shrink VM Script
echo ----------------------------
cd "${0%/*}"

read VM_VERSION < /Users/seamus/GitHub/InstantWP/build/VM_VERSION.txt
VM_ROOT=/Users/seamus/GitHub/InstantWP/core/vm/
VM_FILE="$VM_VERSION".qcow2
VM_IN=$VM_ROOT/$VM_FILE
VM_OUT=$VM_ROOT/small.qcow2 

/Users/seamus/GitHub/InstantWP/core/platform/osx/qemu/bin/qemu-img convert -O qcow2 -c $VM_IN $VM_OUT