combinators
collect: aBlockClosure 
	
	| future |
	future := TKTFuture new.
	self onSuccessDo: [ :got | 
		[ future deploySuccess: (aBlockClosure value: got) ]
			on: Error do: [ :e | future deployFailure: e ] ].
	self onFailureDo: [ :error | future deployFailure: error ].
	^ future