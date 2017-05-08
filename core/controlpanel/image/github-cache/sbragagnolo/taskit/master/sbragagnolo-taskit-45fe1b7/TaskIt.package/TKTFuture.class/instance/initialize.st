initialization
initialize

	super initialize.
	result := TKTFutureNotFinished new.
	runner := TKTCurrentTaskRunner value.

	valueSemaphore := Semaphore new.

	exceptionCallbacks := OrderedCollection new.
	resultCallbacks := OrderedCollection new.