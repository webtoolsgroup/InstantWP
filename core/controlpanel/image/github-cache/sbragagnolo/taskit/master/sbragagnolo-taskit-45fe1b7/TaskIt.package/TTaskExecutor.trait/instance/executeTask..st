executing
executeTask: aTask

	[ aTask value ]
		on: Error
		do: [ :error | self exceptionHandler handleException: error ]