tell application "System Events"
   tell application "qemu-system-i386"
      delay 2
      set visible of first window whose name contains "10022" to false
   end tell
end tell
