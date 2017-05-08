cancelling
cancelTaskExecution: aTaskExecution reason: aReason

	| cancelState activeContext timeoutContext |
	cancelState := TKTCancelledTaskState reason: aReason.
	
	activeContext := aTaskExecution executionProcess suspendedContext.
	timeoutContext := [ cancelState signalCancelledException ] asContext.
	timeoutContext privSender: activeContext.
	aTaskExecution executionProcess suspendedContext: timeoutContext.
	
	aTaskExecution setCancelStateWithReason: aReason