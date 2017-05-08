private
spawnPriority
	^ priority ifNil: [ Processor activePriority ]