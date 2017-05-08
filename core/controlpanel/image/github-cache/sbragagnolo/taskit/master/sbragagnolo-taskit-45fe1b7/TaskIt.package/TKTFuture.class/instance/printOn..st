printing
printOn: aStream

	super printOn: aStream.
	aStream
		nextPutAll: '(finished=';
		nextPutAll: self isFinished asString;
		nextPutAll: ';value=';
		nextPutAll: result asString;
		nextPutAll: ')'