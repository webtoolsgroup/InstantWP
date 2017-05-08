tests
testFutureReturnsAssociatedFutureObject

	| done sema |
	done := false.
	sema := Semaphore new.
	
	[ done := true.
	done ] future onSuccessDo: [ :value | sema signal ].
	
	sema wait.
	self assert: done
	
	