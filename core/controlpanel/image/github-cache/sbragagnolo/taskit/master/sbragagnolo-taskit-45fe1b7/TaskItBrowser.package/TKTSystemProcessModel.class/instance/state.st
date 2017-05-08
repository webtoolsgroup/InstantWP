accessing
state
	^ process isActiveProcess
		ifTrue: [ 'Running' ]
		ifFalse: [ 
			(process isTerminated or: [ process isTerminating ])
				ifTrue: [ 'Terminated' ]
				ifFalse: [ 'Suspended' ] ]