tests
testFutureExecutes

	| done sema |
	done := false.
	sema := Semaphore new.
	[ done := true.
	sema signal ] future.
	sema wait.
	
	self assert: done
	
	