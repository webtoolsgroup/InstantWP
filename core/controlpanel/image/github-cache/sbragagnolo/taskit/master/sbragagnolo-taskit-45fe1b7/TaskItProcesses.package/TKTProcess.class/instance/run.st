running
run
	self ifRunning: [ self error: 'Process is already running' ] ifNotRunning: [ self privateCreatePharoProcess resume ]