starting
stop

	poolWorker stop.
	
	workers do: #stop.
	workers removeAll.
	
	"Empty the free workers queue"
	[freeWorkers isEmpty]
		whileFalse: [ freeWorkers next ]