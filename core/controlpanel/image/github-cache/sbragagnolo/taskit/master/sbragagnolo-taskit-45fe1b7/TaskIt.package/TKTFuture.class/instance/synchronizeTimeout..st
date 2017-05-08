synchronization
synchronizeTimeout: aTimeout
	
	self waitForCompletion: aTimeout.
	^ result valueForFuture: self