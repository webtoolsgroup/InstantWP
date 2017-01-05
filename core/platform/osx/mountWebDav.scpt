tell application "Finder"
    try
        mount volume "http://iwp:iwp@127.0.0.1:10080/webdav/iwpserver.php"
    end try
end tell