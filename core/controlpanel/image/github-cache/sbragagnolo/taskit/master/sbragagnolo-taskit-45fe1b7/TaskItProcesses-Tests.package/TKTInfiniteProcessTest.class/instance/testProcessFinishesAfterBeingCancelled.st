as yet unclassified
testProcessFinishesAfterBeingCancelled
	process run.
	20 milliSeconds wait.
	self assert: process isRunning.
	process cancel.
	20 milliSeconds wait.
	self deny: process isRunning.