accessing
cellColumn: column row: rowIndex
	column id = '#'
		ifTrue: [ ^ FTCellMorph new addMorph: rowIndex asMorph ].
	column id = 'Name'
		ifTrue: [ ^ self nameColumn: column row: rowIndex ].
	column id = 'Sending'
		ifTrue: [ ^ self selectorColumn: column row: rowIndex ].
	column id = 'To'
		ifTrue: [ ^ self receiverColumn: column row: rowIndex ].
	column id = 'With'
		ifTrue: [ ^ self argumentsColumn: column row: rowIndex ].
	column id = 'State'
		ifTrue: [ ^ self stateColumn: column row: rowIndex ].
	"column id = 'Related Task execution'
		ifTrue: [ ^ self taskColumn: column row: rowIndex ]."
	"column id = 'Related Job'
		ifTrue: [ ^ self jobColumn: column row: rowIndex ]."
	self error: 'Invalid column'