tests
testScheduleExecutes

	| done sema |
	done := false.
	sema := Semaphore new.
	[ done := true.
	sema signal ] schedule.
	sema wait.
	
	self assert: done
	
	