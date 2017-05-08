actions
suspend
	process isSuspended
		ifFalse: [ 
			process suspend.
			self announce: (TKTProcessSuspended forProcess: self) ]