testing
ifRunning: aBlockClosure
	^ self isRunning
		ifTrue: [ aBlockClosure value ]
		ifFalse: [ nil ]