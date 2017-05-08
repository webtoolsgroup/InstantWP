as yet unclassified
testProcessFinishingAnnouncementReceivedAfterCancelling
	process run.
	20 milliSeconds wait.
	self assert: processFinishing equals: nil.
	process cancel.
	20 milliSeconds wait.
	self assert: processFinishing equals: process.