accessing
menuColumn: column row: rowIndex
	rowIndex = 0
		ifTrue: [ ^ nil ].	"I could answer a menu without selection, but in this case I will just answer nil, 
		 which means 'no menu'"	"protocols do not have associated menu"
	column id = 'Name'
		ifTrue: [ ^ self nameMenuColumn: column row: rowIndex ].
	^ nil