query
wellKnownProcess
	^ {Smalltalk lowSpaceWatcherProcess.
	WeakArray runningFinalizationProcess.
	Processor backgroundProcess.
	InputEventFetcher default fetcherProcess.
	UIManager default uiProcess.
	Smalltalk globals at: #SoundPlayer ifPresent: [ :sp | sp playerProcess ].
	Smalltalk globals at: #CPUWatcher ifPresent: [ :cw | cw currentWatcherProcess ].
	Delay schedulingProcess} reject: #isNil