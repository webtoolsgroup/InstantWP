parallel running test
testProcessIsRunningJustAfterRunForIfRunning
	process run.
	process ifRunning: [ self assert: true ] ifNotRunning: [ self assert: false ]