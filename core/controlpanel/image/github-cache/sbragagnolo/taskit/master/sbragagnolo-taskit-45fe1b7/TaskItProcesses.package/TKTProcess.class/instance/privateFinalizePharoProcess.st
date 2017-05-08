private
privateFinalizePharoProcess
	self privateEnsureProcess.
	messageSend := nil.
	process
		ifNotNil: [ 
			| pr |
			self announce: (TKTProcessFinishing forProcess: self).
			pr := process.
			process := nil.
			pr terminate ]