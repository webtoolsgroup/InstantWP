parallel running test
testForksForkAtActivePriority
	| priority |
	priority := process
		run;
		priority.
	self assert: priority equals: Processor activePriority