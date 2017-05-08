purging
purge
	
	services := services select: [ :each | each isRunning ]