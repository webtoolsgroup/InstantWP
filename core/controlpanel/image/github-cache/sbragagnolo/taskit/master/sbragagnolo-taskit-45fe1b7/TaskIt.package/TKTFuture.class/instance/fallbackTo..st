combinators
fallbackTo: aFallbackFuture 
	
	| future |
	future := TKTFuture new.
	self onSuccessDo: [ :value | future deploySuccess: value ].
	self onFailureDo: [ :error |
		aFallbackFuture onSuccessDo: [ :value | future deploySuccess: value ].
		aFallbackFuture onFailureDo: [ :value | future deployFailure: value ] ].	
	^ future