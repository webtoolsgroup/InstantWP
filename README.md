# InstantWP

InstantWP is a complete standalone, portable WordPress development environment.

 * [InstantWP Components ](#instantwp-components)
 * [Platform Specific Dependencies ](#platform-specific-dependencies)

# InstantWP Components

## Core Command-Line Components

### IWPServer

### GUI

## Helper Components

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



