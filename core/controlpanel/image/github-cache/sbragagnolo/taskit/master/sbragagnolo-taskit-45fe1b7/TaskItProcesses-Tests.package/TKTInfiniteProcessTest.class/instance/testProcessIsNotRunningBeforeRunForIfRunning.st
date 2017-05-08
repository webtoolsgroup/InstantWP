parallel running test
testProcessIsNotRunningBeforeRunForIfRunning
	process ifRunning: [ self assert: false ] ifNotRunning: [ self assert: true ]