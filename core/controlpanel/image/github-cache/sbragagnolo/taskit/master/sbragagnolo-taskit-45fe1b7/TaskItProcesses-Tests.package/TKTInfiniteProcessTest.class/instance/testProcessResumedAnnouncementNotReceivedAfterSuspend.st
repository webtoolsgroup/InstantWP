parallel running test
testProcessResumedAnnouncementNotReceivedAfterSuspend
	process run.
	20 milliSeconds wait.
	process suspend.
	self assert: processResumed equals: nil