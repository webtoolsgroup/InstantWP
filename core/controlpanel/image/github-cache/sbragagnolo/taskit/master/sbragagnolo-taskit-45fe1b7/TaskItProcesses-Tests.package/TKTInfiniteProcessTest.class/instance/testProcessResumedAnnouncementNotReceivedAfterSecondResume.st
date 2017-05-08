parallel running test
testProcessResumedAnnouncementNotReceivedAfterSecondResume
	process run.
	20 milliSeconds wait.
	process suspend.
	process resume.
	processResumed := nil.
	process resume.
	self assert: processResumed equals: nil