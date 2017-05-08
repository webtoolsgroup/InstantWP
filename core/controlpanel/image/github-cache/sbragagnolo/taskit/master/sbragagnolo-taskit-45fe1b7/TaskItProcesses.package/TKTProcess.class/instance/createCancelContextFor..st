private
createCancelContextFor: aSuspendedContext
	| method |
	method := self class lookupSelector: #privateCancelExecutionFromSuspendedContext:.
	^ (Context
		sender: nil
		receiver: self
		method: method
		arguments: {aSuspendedContext})
		privRefresh;
		yourself