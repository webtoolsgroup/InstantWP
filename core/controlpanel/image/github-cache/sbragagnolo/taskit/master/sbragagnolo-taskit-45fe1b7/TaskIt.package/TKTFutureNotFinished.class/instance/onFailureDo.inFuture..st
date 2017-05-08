callbacks
onFailureDo: aBlockClosure inFuture: aFuture
	
	aFuture addFailureCallback: aBlockClosure