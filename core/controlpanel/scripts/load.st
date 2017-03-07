
"Load the IWP Control Panel code into a dev image"

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

"Load the IWP code itself..."
Gofer new
      directory: '/Users/seamus/GitHub/InstantWP/core/controlpanel/source';
      package: 'InstantWP';
      load.

Gofer new
      directory: '/Users/seamus/GitHub/InstantWP/core/controlpanel/source';
      package: 'IWPWebApplication';
      load.      
