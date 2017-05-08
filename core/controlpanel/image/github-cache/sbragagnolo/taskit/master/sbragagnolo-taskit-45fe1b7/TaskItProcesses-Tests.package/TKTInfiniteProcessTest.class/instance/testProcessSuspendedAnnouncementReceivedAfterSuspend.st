parallel running test
testProcessSuspendedAnnouncementReceivedAfterSuspend
	process run.
	20 milliSeconds wait.
	process suspend.
	self assert: processSuspended equals: process