testing
ifSameProcess: aBlock ifDifferentProcess: anOtherBlock
	^ self isRunningInTheSameProcess
		ifTrue: [ aBlock value ]
		ifFalse: [ anOtherBlock value ]