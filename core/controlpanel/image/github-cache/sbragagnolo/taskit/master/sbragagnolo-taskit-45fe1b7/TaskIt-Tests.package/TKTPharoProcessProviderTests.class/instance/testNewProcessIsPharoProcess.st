tests
testNewProcessIsPharoProcess

	self assert: ((TKTPharoProcessProvider new createProcessDoing: [ 1+1 ]) isKindOf: Process)