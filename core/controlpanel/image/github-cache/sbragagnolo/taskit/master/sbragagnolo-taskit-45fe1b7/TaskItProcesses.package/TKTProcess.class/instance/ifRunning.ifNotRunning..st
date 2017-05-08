testing
ifRunning: aBlock ifNotRunning: anOtherBlock
	^ self isRunning
		ifTrue: [ aBlock value ]
		ifFalse: [ anOtherBlock value ]