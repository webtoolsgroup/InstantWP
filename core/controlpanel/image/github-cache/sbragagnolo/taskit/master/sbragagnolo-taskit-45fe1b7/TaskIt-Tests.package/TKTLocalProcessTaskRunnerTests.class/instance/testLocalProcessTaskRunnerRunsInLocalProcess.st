tests
testLocalProcessTaskRunnerRunsInLocalProcess

	| runner future got |
	runner := TKTLocalProcessTaskRunner new.
	future := runner future: [ Processor activeProcess identityHash ].
	
	future onSuccessDo: [ :value | got := value ].
	future waitForCompletion: 2 seconds.
	
	self assert: got equals: Processor activeProcess identityHash.