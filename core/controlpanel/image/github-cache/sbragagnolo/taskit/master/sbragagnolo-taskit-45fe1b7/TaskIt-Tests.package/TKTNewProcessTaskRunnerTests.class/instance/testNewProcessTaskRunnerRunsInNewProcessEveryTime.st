tests
testNewProcessTaskRunnerRunsInNewProcessEveryTime

	| runner hashes futures hashesSet |
	runner := TKTNewProcessTaskRunner new.
	hashes := AtomicSharedQueue new.
	
	futures := (1 to: 10) collect: [ :i | (runner future: [ Processor activeProcess identityHash ]) onSuccessDo: [ :v | hashes nextPut: v ] ].
	
	futures do: [ :each | each waitForCompletion: 2 seconds ].
	
	hashesSet := Set new.
	[ hashes isEmpty ] whileFalse: [ hashesSet add: hashes next ].
	self assert: hashesSet size equals: 10.