private
privateAskForCancel
	| suspendedContext |
	self privateEnsureNotInSameProcess.
	process suspend.
	suspendedContext := process suspendedContext.
	process suspendedContext: (self createCancelContextFor: suspendedContext).
	process resume