schedulling
future: aTask

	| execution |
	execution := TKTFutureExecution new.
	execution runner: self.
	execution task: aTask.
	
	self scheduleTaskExecution: execution.
	
	^ execution future