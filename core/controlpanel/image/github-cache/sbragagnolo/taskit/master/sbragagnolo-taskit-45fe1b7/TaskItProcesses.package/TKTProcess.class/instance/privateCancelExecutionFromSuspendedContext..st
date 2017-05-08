private
privateCancelExecutionFromSuspendedContext: aSuspendedContext
	self privateEnsureProcess.
	self announce: (TKTProcessCancelledByThirdThread forProcess: self andSuspendedContext: aSuspendedContext).
	self privateFinalizePharoProcess.