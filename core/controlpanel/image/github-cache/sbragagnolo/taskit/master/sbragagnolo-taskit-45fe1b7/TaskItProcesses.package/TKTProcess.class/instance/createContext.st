misc
createContext
	| method |
	method := self class lookupSelector: #privateExecuteAndFinalizeProcess.
	^ (Context
		sender: nil
		receiver: self
		method: method
		arguments: {})
		privRefresh;
		yourself