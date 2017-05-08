accessing
onFailureDo: aBlockClosure inFuture: aFuture
	
	aFuture scheduleCallbackTask: [ aBlockClosure value: value ]