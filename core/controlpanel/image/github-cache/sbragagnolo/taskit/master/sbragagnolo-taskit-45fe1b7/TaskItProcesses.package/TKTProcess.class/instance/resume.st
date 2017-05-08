suspend
resume
	process isSuspended
		ifTrue: [ process resume.
			self announce: (TKTProcessResumed forProcess: self) ]