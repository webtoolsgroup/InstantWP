menu - columns
nameMenuColumn: column row: rowIndex
	| process menu |
	process := (self elementAt: rowIndex) model.
	process ifNil: [ ^ nil ].
	menu := UIManager default newMenuIn: self table for: self.
	menu add: 'Inspect process' target: process selector: #inspect.
	^ menu