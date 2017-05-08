combinators
firstCompleteOf: anotherFuture
	
	| finished future |
	finished := false.
	future := TKTFuture new.
	self onSuccessDo: [ :v |
		finished ifFalse: [
			finished := true.
			future deploySuccess: v ]].
	self onFailureDo: [ :v |
		finished ifFalse: [
			finished := true.
			future deployFailure: v ]].
	anotherFuture onSuccessDo: [ :v |
		finished ifFalse: [
			finished := true.
			future deploySuccess: v ]].
	anotherFuture onFailureDo: [ :v |
		finished ifFalse: [
			finished := true.
			future deployFailure: v ]].
	^ future