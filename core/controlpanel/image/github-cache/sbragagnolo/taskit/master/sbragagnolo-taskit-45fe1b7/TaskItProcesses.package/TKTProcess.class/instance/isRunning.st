testing
isRunning
	^ process isNotNil and: [ process isTerminated not ]