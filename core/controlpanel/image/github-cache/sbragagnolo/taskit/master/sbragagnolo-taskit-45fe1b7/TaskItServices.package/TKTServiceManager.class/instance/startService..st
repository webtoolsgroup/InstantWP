start-stop
startService: aService

	(services anySatisfy: [ :any | any name = aService name ])
		ifTrue: [ self error: ('A service named "{name}" is already registered'
											format: { 'name' -> aService name } asDictionary) ].
										
	aService launch.
	services add: aService.