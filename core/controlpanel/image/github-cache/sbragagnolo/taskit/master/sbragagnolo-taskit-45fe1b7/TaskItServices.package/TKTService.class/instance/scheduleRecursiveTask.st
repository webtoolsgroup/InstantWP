starting
scheduleRecursiveTask

	stopRequested
		ifTrue: [ worker schedule: [ self doStop ] ]
		ifFalse: [
			worker schedule: [ 
				self stepService.
				self scheduleRecursiveTask ]]