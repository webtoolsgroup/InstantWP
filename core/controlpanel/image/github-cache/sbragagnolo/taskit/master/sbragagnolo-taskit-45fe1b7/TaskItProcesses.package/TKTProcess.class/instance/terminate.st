controlling
terminate
	process
		ifNotNil: [ 
			process terminate.
			process := nil ].
	messageSend := nil.