private
installFutureResult: aResult

	result isFinished
		ifTrue: [ self error: 'cannot deploy twice in same future' ].
	result := aResult