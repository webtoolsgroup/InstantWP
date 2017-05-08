running
cancelIfNotRunning: aBlock
	self
		ifRunning: [ self ifSameProcess: [ self privateFinalizePharoProcess ] ifDifferentProcess: [ self privateAskForCancel ] ]
		ifNotRunning: aBlock