#  Creating the IWPServer qcow2 file

Create a new image using qemu-img, 20G in size:

```text
qemu-img create -f qcow2 iwpserver.img 20G
```

Download the Alpine Version 3.5.0 Virtual iso image:

```text
https://fr.alpinelinux.org/alpine/v3.5/releases/x86/alpine-virt-3.5.0-x86.iso
```

Then boot the image using qemu-system-i386 and install Alpine:

```text
./qemu-system-i386 \
-m 1024 \
-hda ./iwpserver.img -boot c \
-cdrom ./alpine-virt-3.5.0-x86.iso
-net nic \
-net user,hostfwd=tcp::10022-:22 \
-redir tcp:10080::80 \
-redir tcp:10020::20 \
-redir tcp:10021::21 \
-monitor telnet:127.0.0.1:1234,server,nowait 
```

Change the root password to 'root'.

Add a user 'iwp' with password 'iwp'.

Install sudo and nano.

Add the iwp user to sudoers.

To get PHP7, edit the /etc/apk/repositories file using an nano and add a line like:

http://dl-6.alpinelinux.org/alpine/edge/community

Obtain the latest index of available packages:

```text
apk update
```

Ssh into the virtual server and install the packages as listed in the installed-packages.md file using the pkg command.

Copy over the files from 2.0.0 via SFTP and reboot.

Also install PHP Composer and WP-CLI.




