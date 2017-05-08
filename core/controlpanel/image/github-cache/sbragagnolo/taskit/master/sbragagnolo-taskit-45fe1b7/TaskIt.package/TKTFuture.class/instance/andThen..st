combinators
andThen: aBlockClosure 
	
	| future |
	future := TKTFuture new.
	self onSuccessDo: [ :got | 
		[ aBlockClosure value: got ]
			on: Error do: [ :e | "Nothing?" ].
		future deploySuccess: got ].
	self onFailureDo: [ :error | future deployFailure: error ].
	^ future