executing
performTaskExecution: aTaskExecution

	aTaskExecution setRunningState.
	aTaskExecution doExecuteTask.
	aTaskExecution setFinishedState.