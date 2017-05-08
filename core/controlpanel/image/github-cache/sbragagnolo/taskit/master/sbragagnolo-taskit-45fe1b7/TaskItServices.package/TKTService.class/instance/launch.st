private
launch

	self name ifNil: [ self error: 'A name should be provided for a service' ].
	
	worker name: self name.
	worker start.
	worker schedule: [ self setUp ].
	self scheduleRecursiveTask.