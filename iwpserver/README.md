#  Creating the IWPServer qcow2 file

Create a new image using qemu-img, 20G in size:

```text
qemu-img create -f qcow2 iwpserver.img 20G
```

Download the FreeBSD 8.4 iso image:

```text
http://ftp-archive.freebsd.org/pub/FreeBSD-Archive/old-releases/i386/ISO-IMAGES/8.4/FreeBSD-8.4-RELEASE-i386-bootonly.iso
```

Then boot the image using qemu-system-i386 and install FreeBSD:

```text
./qemu-system-i386 \
-m 1024 \
-hda ./iwpserver.img -boot c \
-cdrom ./FreeBSD-8.4-RELEASE-i386-bootonly.iso
-net nic \
-net user,hostfwd=tcp::10022-:22 \
-redir tcp:10080::80 \
-redir tcp:10020::20 \
-redir tcp:10021::21 \
-monitor telnet:127.0.0.1:1234,server,nowait 
```

Add a user 'iwp' with password 'iwp'.

Edit the pkg /usr/local/etc/pkg/repos/FreeBSD.conf as per the included file.

Install sudo and nano.

Add the iwp user to sudoers.

Ssh into the virtual server and install the packages as listed in the installed-packages.md file using the pkg command.

Copy over the /usr/local/etc via SFTP and reboot.





