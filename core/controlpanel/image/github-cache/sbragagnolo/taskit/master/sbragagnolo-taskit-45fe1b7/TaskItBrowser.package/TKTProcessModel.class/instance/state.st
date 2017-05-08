accessing
state
	^ process
		ifNotNil: [ :p | 
			p
				ifRunning: [ 
					p isSuspended
						ifTrue: [ 'Suspended' ]
						ifFalse: [ 'Running' ] ]
				ifNotRunning: 'NotRunning' ]