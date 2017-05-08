deployment
deploySuccess: aValue

	self installFutureResult: (TKTFutureSuccess new value: aValue).
	resultCallbacks do: [ :each | 
		self scheduleCallbackTask: [ each value: aValue ] ].
	valueSemaphore signal.