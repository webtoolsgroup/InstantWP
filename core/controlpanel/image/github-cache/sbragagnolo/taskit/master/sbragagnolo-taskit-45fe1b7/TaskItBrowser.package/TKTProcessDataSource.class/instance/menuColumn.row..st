accessing
menuColumn: column row: rowIndex
	rowIndex = 0 ifTrue: [ 
		"I could answer a menu without selection, but in this case I will just answer nil, 
		 which means 'no menu'"
		^ nil ].

	"protocols do not have associated menu"
	column id = 'Name' ifTrue: [ ^ self nameMenuColumn: column row: rowIndex ].
	column id = 'Sending' ifTrue: [ ^ self selectorMenuColumn: column row: rowIndex ].	
	column id = 'To' ifTrue: [ ^ self receiverMenuColumn: column row: rowIndex ].	
	column id = 'With' ifTrue: [ ^ self argumentsMenuColumn: column row: rowIndex ].	
	"column id = 'Related Task execution' ifTrue: [ ^ self taskMenuColumn: column row: rowIndex ].	"
	"column id = 'Related Job' ifTrue: [ ^ self jobMenuColumn: column row: rowIndex ].	"
	^ nil	