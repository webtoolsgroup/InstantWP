event-handling
noteProcessCancelled: aProcessAnnouncement
	self elements
		detect: [ :p | p model = aProcessAnnouncement process ]
		ifFound: [ :p | p hasBeenCancelled ].
	self tableRefresh