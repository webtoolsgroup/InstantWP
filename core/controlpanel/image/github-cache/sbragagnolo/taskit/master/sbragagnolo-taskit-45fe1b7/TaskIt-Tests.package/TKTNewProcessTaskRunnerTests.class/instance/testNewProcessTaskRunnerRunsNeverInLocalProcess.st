tests
testNewProcessTaskRunnerRunsNeverInLocalProcess

	| runner hashes futures |
	runner := TKTNewProcessTaskRunner new.
	hashes := AtomicSharedQueue new.
	
	futures := (1 to: 10) collect: [ :i | (runner future: [ Processor activeProcess identityHash ]) onSuccessDo: [ :v | hashes nextPut: v ] ].
	
	futures do: [ :each | each waitForCompletion: 2 seconds ].
	
	[ hashes isEmpty ] whileFalse: [ 
		self assert: hashes next ~= Processor activeProcess identityHash ].