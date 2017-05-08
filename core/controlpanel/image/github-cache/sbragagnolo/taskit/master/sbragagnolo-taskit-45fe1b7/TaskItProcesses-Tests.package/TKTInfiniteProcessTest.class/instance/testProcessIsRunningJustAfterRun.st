parallel running test
testProcessIsRunningJustAfterRun
	process run.
	self assert: process isRunning.