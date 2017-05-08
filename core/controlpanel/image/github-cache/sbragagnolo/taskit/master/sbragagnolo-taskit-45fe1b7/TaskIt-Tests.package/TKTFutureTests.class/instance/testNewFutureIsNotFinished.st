tests-termination
testNewFutureIsNotFinished

	| future |
	future := TKTFuture new.	
	self assert: future isFinished not