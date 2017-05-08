evaluating
doExecuteTask
	
	[ future deploySuccess: super doExecuteTask ] 
		on: Error
		do: [ :error | future deployFailure: error ]