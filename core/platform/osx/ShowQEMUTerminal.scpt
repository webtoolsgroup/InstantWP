tell application "System Events"
   tell application "Terminal"
      delay 2
      set visible of first window whose name contains "10022" to true
   end tell
end tell
