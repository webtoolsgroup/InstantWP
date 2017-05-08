combinators
on: anError do: aBlockClosure 
	
	| future |
	future := TKTFuture new.
	self onSuccessDo: [ :got | future deploySuccess: got ].
	self onFailureDo: [ :error | 
		(anError handles: error)
			ifTrue: [ [future deploySuccess: (aBlockClosure value: error)]
							on: Error do: [ :e | future deployFailure: e ]  ]
			ifFalse: [ future deployFailure: error ] ].
	^ future