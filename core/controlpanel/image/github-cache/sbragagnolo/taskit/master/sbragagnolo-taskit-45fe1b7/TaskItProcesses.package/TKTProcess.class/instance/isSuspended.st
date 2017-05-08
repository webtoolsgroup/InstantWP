testing
isSuspended
	^ process ifNotNil: [ :p | p isSuspended ] ifNil: [ false ]