combinators
select: aBlockClosure 
	
	^ self collect: [ :r | (aBlockClosure value: r) ifTrue: [ r ] ifFalse: [ NotFound signalFor: aBlockClosure ] ].
	