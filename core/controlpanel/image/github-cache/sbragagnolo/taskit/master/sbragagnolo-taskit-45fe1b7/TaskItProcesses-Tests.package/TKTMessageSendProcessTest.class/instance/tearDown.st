setup
tearDown
	super tearDown.
	TKTProcess uniqueAnnouncer unsubscribe: self.
	TKTProcess resetAnnouncer