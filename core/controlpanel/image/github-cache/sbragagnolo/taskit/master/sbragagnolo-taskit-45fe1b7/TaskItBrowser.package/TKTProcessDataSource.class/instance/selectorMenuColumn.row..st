menu - columns
selectorMenuColumn: column row: rowIndex
	| selector receiver menu |
	selector := (self elementAt: rowIndex) selector asSymbol.
	receiver := (self elementAt: rowIndex) receiver.
	menu := UIManager default newMenuIn: self table for: self.
	menu add: 'Method' target: [ SystemNavigation default browseHierarchy: receiver class selector: selector ] selector: #value.
	menu add: 'Implementors' target: [ SystemNavigation default browseAllImplementorsOf: selector ] selector: #value.
	^ menu