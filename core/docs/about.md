# About InstantWP

Version 5.0.0-rc5

InstantWP is a complete standalone, portable WordPress development environment. It turns any Windows or Mac machine into a WordPress development server. It should even run from a USB key.

 * [Usage ](#usage)
 * [License ] (#license)

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

## License

InstantWP is released under the GPL v3. See the included LICENSE file.

You can use InstantWP for any purposes for an unlimited period of time. 

InstantWP is provided free of charge.

InstantWP may be installed and used on any number of systems.

THIS SOFTWARE IS PROVIDED “AS IS” BY CORVIDEON LTD AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.

All trademarks and images are copyright of their respective owners.

Software licenses are provided in the respective software directories or links are included in the documentation below.

InstantWP is not associated with Automattic or WordPress.com.


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

## Executables

 * The Perl based executables are built using ActiveState's PerlApp.
 * The GUI is built using PureBasic [http://www.purebasic.com]() 
 * The F# components are build using Xamarin Studio on macOS and Visual Studio on Windows.

