parallel running test
testProcessStartedAnnouncementReceivedAfterRun
	process run.
	20 milliSeconds wait.
	self assert: processStarted equals: process