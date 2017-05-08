accessing
task
	^ task
		ifNotNil: [ task ]
		ifNil: [ 'NonTaskRelated' ]