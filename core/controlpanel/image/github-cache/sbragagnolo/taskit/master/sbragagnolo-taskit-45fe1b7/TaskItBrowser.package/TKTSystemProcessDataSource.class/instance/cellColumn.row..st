accessing
cellColumn: column row: rowIndex
	column id = '#'
		ifTrue: [ ^ FTCellMorph new addMorph: rowIndex asMorph ].
	column id = 'Name'
		ifTrue: [ ^ self nameColumn: column row: rowIndex ].
	column id = 'State'
		ifTrue: [ ^ self stateColumn: column row: rowIndex ].
	column id = 'Priority'
		ifTrue: [ ^ self priorityColumn: column row: rowIndex ].
	self error: 'Invalid column'