event-handling
noteProcessFinished: aProcessAnnouncement
	self elements
		detect: [ :p | p model = aProcessAnnouncement process ]
		ifFound: [ :p | p hasFinished ].
	self tableRefresh