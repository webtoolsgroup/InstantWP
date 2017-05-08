tests
testWorkerExceptionIsHandledByExceptionHandler

	| error worker |
	worker := TKTWorker new.
	worker exceptionHandler: (TKTTestExceptionHandler new).
	worker start.
	
	error := Error new.
	worker schedule: [ error signal ].
	
	"We wait until the error is signaled, trapped and we have it."
	100 milliSeconds wait.
	self assert: worker exceptionHandler error equals: error