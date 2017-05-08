initialization
initialize

	super initialize.
	stopRequested := false.
	stopCallbacks := Set new.
	
	worker := TKTWorker new.