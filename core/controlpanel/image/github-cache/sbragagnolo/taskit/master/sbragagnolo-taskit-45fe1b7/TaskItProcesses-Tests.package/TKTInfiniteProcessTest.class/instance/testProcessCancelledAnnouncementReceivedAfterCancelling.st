as yet unclassified
testProcessCancelledAnnouncementReceivedAfterCancelling
	process run.
	20 milliSeconds wait.
	self assert: processCancelled equals: nil.
	process cancel.
	20 milliSeconds wait.
	self assert: processCancelled equals: process.
	self assert: contextCancelled isNotNil