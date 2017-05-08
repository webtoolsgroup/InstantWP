parallel running test
testProcessFinishes
	process run.
	20 milliSeconds wait.
	self deny: process isRunning.