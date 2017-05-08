parallel running test
testProcessResumedAnnouncementReceivedAfterResume
	process run.
	20 milliSeconds wait.
	process suspend.
	process resume.
	self assert: processResumed equals: process