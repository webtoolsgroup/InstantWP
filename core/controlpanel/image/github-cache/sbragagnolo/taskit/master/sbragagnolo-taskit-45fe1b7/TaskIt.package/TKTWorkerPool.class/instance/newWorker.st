schedulling
newWorker
	
	^ workers add: (TKTWorker new
		name: 'Global Pool Worker #', (self size + 1) asString;
		start)