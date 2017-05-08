event-handling
noteProcessResumed: aProcessAnnouncement
	self elements
		detect: [ :p | p model = aProcessAnnouncement process ]
		ifFound: [ :p | p hasBeenResumed ].
	self tableRefresh