schedulling
getAvailableWorker

	| worker |
	worker := freeWorkers nextIfNone: [
			self size < poolMaxSize
				ifTrue: [ self newWorker ]
				ifFalse: [ freeWorkers next ] ].
	^ worker