"Load the IWP Control Panel code into a production image"



"Load OSProcess..."

Gofer new
	squeaksource: 'OSProcess';
	package: 'OSProcess';
	load.
	
"Load the INIFile code"

Gofer it
  smalltalkhubUser: 'TorstenBergmann' project: 'INIFile';
  configuration;
  loadStable.

"Load the IWP code itself..."

Gofer new
      directory: '/Users/seamus/GitHub/InstantWP/core/controlpanel/source';
      package: 'InstantWP';
      load.
      
"Run the startup script"

IWPConfig initialize.

IWPLoadingScreen new openWorldWithSpec.

Smalltalk snapshot: true andQuit: false.