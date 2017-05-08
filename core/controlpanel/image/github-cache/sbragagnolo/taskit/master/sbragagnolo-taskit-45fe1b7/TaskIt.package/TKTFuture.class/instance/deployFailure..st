deployment
deployFailure: aValue

	self installFutureResult: (TKTFutureFailure new value: aValue).
	exceptionCallbacks do: [ :each | 
		self scheduleCallbackTask: [ each value: aValue ] ].
	valueSemaphore signal.