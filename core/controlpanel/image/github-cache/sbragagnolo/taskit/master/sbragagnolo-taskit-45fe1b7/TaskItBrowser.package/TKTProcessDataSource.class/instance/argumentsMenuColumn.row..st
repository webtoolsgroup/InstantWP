menu - columns
argumentsMenuColumn: column row: rowIndex
	| arguments menu |
	arguments := (self elementAt: rowIndex) arguments.
	arguments
		ifNotEmpty: [ 
			menu := UIManager default newMenuIn: self table for: self.
			menu add: 'Inspect arguments' target: arguments selector: #inspect.
			^ menu ].
	^ nil