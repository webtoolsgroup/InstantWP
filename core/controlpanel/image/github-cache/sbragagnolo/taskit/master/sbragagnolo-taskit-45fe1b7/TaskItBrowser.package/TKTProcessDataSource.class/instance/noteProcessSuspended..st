event-handling
noteProcessSuspended: aProcessAnnouncement
	self elements
		detect: [ :p | p model = aProcessAnnouncement process ]
		ifFound: [ :p | p hasBeenSuspended ].
	self tableRefresh