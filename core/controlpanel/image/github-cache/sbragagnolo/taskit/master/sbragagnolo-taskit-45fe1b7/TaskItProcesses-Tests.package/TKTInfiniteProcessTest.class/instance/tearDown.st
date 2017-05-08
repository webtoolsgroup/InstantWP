as yet unclassified
tearDown
	super tearDown.
	TKTProcess uniqueAnnouncer unsubscribe: self.
	TKTProcess resetAnnouncer.
	process
		ifNotNil: [ 
			process cancelIfNotRunning: [  ].
			20 milliSeconds wait ]