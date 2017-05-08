initialization
initialize

	super initialize.

	workers := Set new.
	freeWorkers := AtomicSharedQueue new.

	poolWorker := TKTWorker new.