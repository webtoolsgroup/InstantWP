parallel running test
testProcessSuspendedAnnouncementNotReceivedAfterResume
	process run.
	20 milliSeconds wait.
	process suspend.
	processSuspended := nil.
	process resume.
	self assert: processSuspended equals: nil