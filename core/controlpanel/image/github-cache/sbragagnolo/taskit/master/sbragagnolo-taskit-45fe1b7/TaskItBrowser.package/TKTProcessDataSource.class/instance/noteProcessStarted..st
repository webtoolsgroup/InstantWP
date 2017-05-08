event-handling
noteProcessStarted: aProcessAnnouncement
	self elements add: (TKTProcessModel on: aProcessAnnouncement process).
	self tableRefresh