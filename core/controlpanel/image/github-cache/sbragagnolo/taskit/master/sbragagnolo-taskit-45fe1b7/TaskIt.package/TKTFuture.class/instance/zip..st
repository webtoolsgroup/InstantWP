combinators
zip: secondFuture
	
	^ self zip: secondFuture with: [ :a :b | Array with: a with: b ]