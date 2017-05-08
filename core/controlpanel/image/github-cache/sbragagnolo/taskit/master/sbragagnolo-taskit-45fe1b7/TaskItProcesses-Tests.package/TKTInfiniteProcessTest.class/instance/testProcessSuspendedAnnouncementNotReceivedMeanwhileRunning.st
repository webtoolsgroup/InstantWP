parallel running test
testProcessSuspendedAnnouncementNotReceivedMeanwhileRunning
	process run.
	20 milliSeconds wait.
	self assert: processResumed equals: nil