combinators
flatCollect: aBlockClosure 
	
	| future |
	future := TKTFuture new.
	self onSuccessDo: [ :got | [(aBlockClosure value: got)
		onSuccessDo: [ :got2 | future deploySuccess: got2 ];
		onFailureDo: [ :error | future deployFailure: error ]]
				on: Error do: [ :e | future deployFailure: e ] ].
	self onFailureDo: [ :error | future deployFailure: error ].
	^ future