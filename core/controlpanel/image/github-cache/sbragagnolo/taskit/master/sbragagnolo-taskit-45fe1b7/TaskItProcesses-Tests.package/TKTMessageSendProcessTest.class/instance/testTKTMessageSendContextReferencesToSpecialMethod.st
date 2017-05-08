low-level tests
testTKTMessageSendContextReferencesToSpecialMethod
	| context |
	context := process createContext.
	self assert: context method selector equals: #privateExecuteAndFinalizeProcess