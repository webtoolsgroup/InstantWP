private
privateExecution
	self privateEnsureProcess.
	self announce: (TKTProcessStarted forProcess: self).
	messageSend value.
	