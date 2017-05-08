tests
testProcessFinishingAnnouncementReceivedAfterRun
	process run.
	20 milliSeconds wait.
	self assert: processFinishing equals: process