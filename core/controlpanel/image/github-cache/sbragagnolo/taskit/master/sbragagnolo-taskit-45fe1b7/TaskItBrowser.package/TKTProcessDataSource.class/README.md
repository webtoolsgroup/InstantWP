TKTProcessDataSource is a datasource class for a fast table.
It creates ProcessModels for each tktprocess existant in the system during the initialization, and it registers to the TKTProcess announcer for learned about process state changes and new processes. 

It allows 

	With the TKTProcess:
			inspect it
			pause/resume it
			cancel it
			
	Since a process is spawned only wiht a MessageSend,
	With the MessageSend related to the TKTProcess:
		Inspect the receiver
		Inspect the method to call
		Inspect the rest of implementors of the selector.
		Inspect the arguments
		
	Since this is TaskIT, usually a process is related with a Task Execution and/or a Job
	
		Inspect the TaskExecution object (with the execution callbacks).
		Inspect the Job to know what is this process for.