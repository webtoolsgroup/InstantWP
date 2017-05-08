parallel running test
testProcessSuspendedAnnouncementNotReceivedAfterSecondSuspend
	process run.
	20 milliSeconds wait.
	process suspend.
	processSuspended := nil.
	process suspend.
	self assert: processResumed equals: nil