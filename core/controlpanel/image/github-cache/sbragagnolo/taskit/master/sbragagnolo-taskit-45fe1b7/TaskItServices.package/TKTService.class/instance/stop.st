starting
stop
	
	| futureStop |
	stopRequested := true.
	futureStop := TKTFuture new.
	self onStoppedDo: [ :v | futureStop deploySuccess: self ].
	^ futureStop