synchronization
waitForCompletion: aTimeout

	self isFinished ifFalse: [ | expired |
		expired := valueSemaphore waitTimeoutMSecs: aTimeout asMilliSeconds.
		expired ifTrue: [ TKTTimeoutException signal ] ]