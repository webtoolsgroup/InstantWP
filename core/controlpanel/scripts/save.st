"Load the IWP Control Panel code into a production image"

"Load CommandShell"
Gofer new
	squeaksource: 'CommandShell';
	package: 'CommandShell-Piping';
	load.

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

"Load the Seaside Bootstrap"
Gofer new
    url: 'http://smalltalkhub.com/mc/TorstenBergmann/Bootstrap/main';
    package: 'ConfigurationOfBootstrap';
    load.

(Smalltalk at: #ConfigurationOfBootstrap) project stableVersion load.

"Load the Webbrowser package"
Metacello new
    smalltalkhubUser: 'TorstenBergmann' project: 'WebBrowser';
    configuration: 'WebBrowser';
    load.

"Load TaskIt task runner"
Metacello new
  baseline: 'TaskIt';
  repository: 'github://sbragagnolo/taskit';
  load.


"Load the IWP code itself..."
Gofer new
      directory: '/Users/seamus/GitHub/InstantWP/core/controlpanel/source';
      package: 'InstantWP-Common';
      load.
      
Gofer new
      directory: '/Users/seamus/GitHub/InstantWP/core/controlpanel/source';
      package: 'InstantWP-ControlPanel';
      load.

Gofer new
      directory: '/Users/seamus/GitHub/InstantWP/core/controlpanel/source';
      package: 'InstantWP-WebControlPanel';
      load.   
      
"Run the startup script"
IWPConfig initialize.
IWPLoadingScreen new openWorldWithSpec.
Smalltalk snapshot: true andQuit: false.
