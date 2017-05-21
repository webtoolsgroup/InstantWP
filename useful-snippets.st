


IWPConfig initialize.
IWPLoadingScreen new openDialogWithSpec.
[ IWPProgressBar showStartUpProgressBar ] fork.

DisplayScreen hostWindowTitle: 'InstantWP Control Panel'

IWPWebServer start.
IWPWebServer stop.


IWPEnvironment startUp.
IWPControlPanel new openDialogWithSpec.

	"Add a workaround for an OSProcess bug see 	https://pharo.fogbugz.com/f/cases/18359/Problem-with-DelayExperimentalSpinScheduler-and-delay"
	Delay delaySchedulerClass: DelayMillisecondScheduler.

20 timesRepeat: [ 
	Transcript show: (PipeableOSProcess command: 'pwd') output
].

aFuture := [ self aboutBtn disable ] future.
	aFuture onSuccessDo: [ :result | 
		1 second wait.
		self aboutBtn enable.
	].

schedule: aTask

	| delay |
	
	delay := Delay forMilliseconds: 500.
	delay wait.
	pool ifNotNil: [ 
		pool schedule: [ aTask ].
 	]

IWPConfig reInit.

"[ OSProcess command: ' exec ', aPath ] schedule"
	| worker |
	
	worker := TKTWorker new.
	worker exceptionHandler: [ 
		worker stop. 
		^self ].
	worker start.
	worker schedule: [ OSProcess command: ' ', aPath ].
	worker stop.
	
	
doOSProcessOnMacOS: aPath
	
	"Reset OSProcess to stop image freezing"
	OSProcessAccessor initialize.

	[ OSProcess command: ' exec ', aPath ] schedule
	  
doOSProcessOnMacOS: aPath

	"[ OSProcess command: ' exec ', aPath ] schedule"
	| aRunner |
	
	aRunner := TKTNewProcessTaskRunner new.
	aRunner exceptionHandler: [ TKTDebuggerExceptionHandler new ].
	aRunner schedule: [ OSProcess command: ' ', aPath ].

(IWPActions new doStartSSHScriptAction)



	html tbsNavbarButton 
		callback: ['http://www.google.ie'];
		with: 'SSH Console'
		
html
		listItem: [ 
			IWPMenuWebComponent new renderWordPressMenuItemOn: html.
			html text: ': Access to the local WordPress installation' ].
	html listItem: [ 
			IWPMenuWebComponent new renderNetworkPortsMenuItemOn: html.
			html text: ': Details about InstantWP Network Ports'  ].
	html listItem: [ 
			IWPMenuWebComponent new renderCLIMenuItemOn: html.
			html text: ': Various CLI tools'  ].
	html listItem: [ 
			IWPMenuWebComponent new renderFilesMenuItemOn: html.
			html text: ': Accessing files and folders in the IWPServer VM'  ].
	html listItem: [ 
			IWPMenuWebComponent new renderVMMenuItemOn: html.
			html text: ': Accessing the IWPServer VM'  ].
	html listItem: [ 
			IWPMenuWebComponent new renderDocsMenuItemOn: html.
			html text: ': Links to help related information'  ]