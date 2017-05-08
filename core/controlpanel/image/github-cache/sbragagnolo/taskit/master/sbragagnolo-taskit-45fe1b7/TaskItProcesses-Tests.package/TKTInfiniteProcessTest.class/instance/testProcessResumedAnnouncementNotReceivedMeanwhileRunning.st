parallel running test
testProcessResumedAnnouncementNotReceivedMeanwhileRunning
	process run.
	20 milliSeconds wait.
	self assert: processResumed equals: nil