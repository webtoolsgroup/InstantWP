accessing
priority: aPriority
	priority := aPriority.
	self ifRunning: [ process priority: aPriority ]