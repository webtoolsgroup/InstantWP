# InstantWP Documentation

InstantWP is a complete standalone, portable WordPress development environment. It turns any Windows or Mac machine into a WordPress development server. It should even run from a USB key.

 * [Overview ](#overview)
 * [Usage ](#usage)
 * [Downloads and Releases] (#downloads-and-releases)
 * [License ] (#license)
 * [InstantWP Components ](#instantwp-components)
 * [Platform Specific Dependencies ](#platform-specific-dependencies)
 * [Building ](#building)

## Overview

InstantWP contains a virtual webserver wrapped in a simple user interface that should that start up in under a minute. 

* InstantWP has been under development since 2008 and was first developed as a WordPress teaching tool. 
* InstantWP has been downloaded over a million times.
* InstantWP is used worldwide in universities, schools and businesses for teaching, developing and working with WordPress.

### Design Principles

The main design principles behind InstantWP are:

1. InstantWP is portable. No software should be installed on the user's machine outside of the InstantWP folder. 
2. InstantWP is simple. Using InstantWP should be easy for a beginner. 
3. InstantWP is quick. It should be fast to create quick, disposable WordPress installs for testing, teaching or development.
3. InstantWP is useful. InstantWP should be a power tool for WordPress beginners and experts, theme designers or software developers. 
4. InstantWP is free, like WordPress :)


For more information please visit the main InstantWP website: [http://www.instantwp.com ]() 

For the source code, please visit the InstantWP GitHub repo:
[https://github.com/corvideon/InstantWP ]() 


## Usage

No install of InstantWP is necessary, simply download, unzip and run.

 * Download the latest release.
 * Unzip the IWP folder somewhere convenient, such as your desktop.
 * On Windows start InstantWP by double-clicking on Start-InstantWP.bat
 * On macOS, start InstantWP by double-clicking on Start-InstantWP.
 * A command window will open and start the web-server. Once this is complete the GUI will open. 

## GUI

![InstantWP GUI](./images/IWP-gui-5.0.0-rc3.png)

The GUI contains the following buttons:

 * WordPress Frontpage

 This will open the local WordPress installation in your default browser.
 
 * WordPress Admin

 This will open the local WordPress administration backed in your default browser. The username is ‘admin’ and the password is ‘password’ as specified.
 * Plugins Folder

 This will open the local WordPress plugins folder in Windows Explorer or macOS Finder.
 
 * Themes Folder

 This will open the local WordPress themes folder in Windows Explorer or macOS Finder.
 
* MySQL Database Admin

	This opens PHPMyAdmin in your default browser so that you can administrate the underlying MySQL database. The user is 'root' with a blank password.
	
* About and Documentation open the documentation.


## Downloads and Releases

See the InstantWP Releases page:

[https://github.com/corvideon/InstantWP/releases]() 

## License

InstantWP is released under the GPL v3. See the included LICENSE file.

You can use InstantWP for any purposes for an unlimited period of time. 

InstantWP is provided free of charge.

InstantWP may be installed and used on any number of systems.

THIS SOFTWARE IS PROVIDED “AS IS” BY CORVIDEON LTD AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.

All trademarks and images are copyright of their respective owners.

Software licenses are provided in the respective software directories or links are included in the documentation below.

InstantWP is not associated with Automattic or WordPress.com.

## InstantWP Components

### InstantWP Command-Line 

InstantWP comes with a command-line tool called iwpcli / iwpcli.exe.

Check if IWP is running and print out a status message:

```text
iwpcli status 
```

Start InstantWP:

```text
iwpcli start
```

Quit InstantWP:

```text
iwpcli quit
```

Open WordPress frontpage:

```text             
iwpcli wpfrontpage
```

Open WordPress Dashboard:
 
```text             
iwpcli wpadmin
```

Open the plugins folder:

```text
iwpcli plugins
```

Open the themes folder:

```text
iwpcli themes
```
        
Open PHPMyAdmin:

```text
iwpcli mysql
```
        
Open IWP documentation:       

```text             
iwpcli docs
```
           
Open IWP about page:           
   
```text           
iwpcli about
```
              
Open up an SSH session on the IWP VM:
   
```text       
iwpcli ssh
```
            
Open up the QEMU VM Monitor shell:
 
```text
iwpcli monitor
```

Restart the Apche web server and MySQLL
              
```text
iwpcli restart_apache
```

Mount the IWPServer WebDav Volume (macOS only):

```text
iwpcli mount_webdav
```
       
Unmount the IWPServer WebDav Volume (macOS only): 

```text
iwpcli unmount_webdav
```       

### IWPServer

The built in virtual webserver is called IWPServer.
IWPServer is based on FreeBSD 8.4.
Please see the iwpserver folder README in the GitHub repo for more details.

### Helper Components

 * IWPQEMUTelnet

 When QEMU is running, it provides a monitor console for interacting with QEMU. This command-line executable opens a telnet connection to communicate with the QEMU monitor process. The executable is written in F# and uses the Rebex .net libraries as listed below.

* IWPSSHCommand

	This command-line executable opens an SSH connection and runs a command. The executable is written in F# and uses the Rebex .net libraries as listed below.


## Platform Specific Dependencies

These dependencies are bundled as binaries into a folder called 'platform' for each operating system and they may be downloaded seperately here. They are listed here for convenience, you don't need to download or compile these files seperately unless you wish to.

### Open Source Dependencies

**macOS Dependencies**

 * QEMU
 
 	QEMU is an open source machine emulator and virtualizer, as installed and compiled by Homebrew. 
 	
 	* Version 2.8.0
	* Homepage: [http://wiki.qemu.org/Main_Page]() 
	* Source code: [http://wiki.qemu-project.org/download/qemu-2.8.0.tar.bz2]()
	* License: [http://wiki.qemu.org/License]() (GPL v2)

	The following Dynamic Library for macOS are also shipped with QEMU:
	
	 * GLib 2.0
	 * GnuMP 1.0
	 * GnuTLS 3.0
	 * Nettle 6.0
	 * Libjpeg 8
	 * Pixman 1.0
	 * Libpng 1.6
	 * Libtasn 1.0.

**Windows Dependencies**

 * QEMU
 
 These QEMU Binaries for Windows (32 bit) are as supplied by Stefan Weil.
 	
 	* Version 2.5.0
	* Homepage: [https://qemu.weilnetz.de/]() 
	* Source code: [http://repo.or.cz/w/qemu/ar7.git/]()
	* License: [http://wiki.qemu.org/License]() (GPL v2)
	
 * Ansicon

	ANSICON provides ANSI escape sequence recognition for Windows console programs (both 32- (x86) and 64-bit (x64)). 
	
	* Version 1.6.6
	* Homepage: [http://adoxa.altervista.org/ansicon/]() 
	* Source code: [https://github.com/adoxa/ansicon/]()
	* License: [https://github.com/adoxa/ansicon/blob/master/LICENSE.txt]() 
	
		
 * PuTTY / PLink

 PuTTY is a Windows SSH / Telnet client. Plink is a command-line interface to the PuTTY back end.
 
   * Version 0.67
	* Homepage: [http://www.chiark.greenend.org.uk/~sgtatham/putty/]() 
	* Source code: [https://the.earth.li/~sgtatham/putty/latest/putty-0.67.tar.gz]()
	* License: [http://www.chiark.greenend.org.uk/~sgtatham/putty/licence.html](MIT) 
 

### Closed Source Dependencies

 * NirCmd

	NirCmd is a small Windows command-line utility that allows you to do some useful tasks without displaying any user interface.
	
 	* Version 2.8.1
	* Homepage: [http://www.nirsoft.net/utils/nircmd.html]() 
	* License: Distributable Freeware.

* Rebex .Net 

	The QEMU telnet and ssh components use the closed source Rebex .Net Telnet and SSH libraries below:

 * Rebex File Server 2016R1.1
 * Rebex Ssh Pack 2016R1.1

	Corvideon is licensed to redistribute the component DLLs with InstantWP royalty free. See [http://www.rebex.net](http://www.rebex.net) for further information.

## Building

 * The Perl based executables are built using ActiveState's PerlApp.
 * The GUI is built using PureBasic [http://www.purebasic.com]() 
 * The F# components are build using Xamarin Studio on macOS and Visual Studio on Windows.

