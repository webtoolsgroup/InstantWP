as yet unclassified
forProcess: aProcess andSuspendedContext: aSuspendedContext
	^ (super forProcess: aProcess)
		suspendedContext: aSuspendedContext;
		yourself