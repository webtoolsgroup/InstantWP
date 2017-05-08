combinators
zip: secondFuture with: aBlock
	
	^ self flatCollect: [ :result1 | secondFuture collect: [ :result2 | aBlock value: result1 value: result2 ] ]